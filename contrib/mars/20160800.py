#!/usr/bin/env python3
""" coding: utf-8
# Il y a six types de fichiers à traiter.
#
# nb images | tz      | poni
# ----------|---------|
# 5         |  -1     | x
# 5         |  0      | scan3.poni
# 5         |  x(-1)  | x
# 5         |  x(0)   | scan3.poni
# 1         |  -1     | x
# 1         |  0      | scan3.poni

# In[7]:

# # extraction des scans avec 6 positions en tx, quelque soit tz.
# # LONG
# import glob

# files = glob.glob(os.path.join(ROOT, "*.nxs"))

# def is_ok(filename: str) -> bool:
#     with h5py.File(filename, mode='r') as f:
#         for imgs, tx, tz in zip(get_images(f), get_tx(f), get_tz(f)):
#             return True if tx.shape[0] == 6 else False

# good = [f for f in files if is_ok(f)]
# print(good)
"""

from typing import Iterator, List, Tuple

import os
import functools

from collections import namedtuple

import h5py
import numpy
import pylab
import pyFAI

from fabio.edfimage import edfimage

from pyFAI.goniometer import GeometryTransformation, GoniometerRefinement
from pyFAI.gui import jupyter


ROOT = "/home/experiences/instrumentation/picca/jupyter/mars/20160800/"
PUBLISHED = os.path.join(ROOT, "published-data")

CALIB = os.path.join(ROOT, "scan_3_01.nxs")

H5OptionalItemValue = namedtuple('H5OptionalItemValue', ['path', 'default'])

MetaData = namedtuple("MetaData", ["img", "tx", "tz"])

MultiCalib = namedtuple("MultiCalib", ["filename", "h5path", "idxs",
                                       "calibrant", "detector", "wavelength"])

Parameter = namedtuple("Parameter", ["name", "value", "bounds"])


def visit_images(acc: List[h5py.Dataset], _name: str, obj) -> None:
    """extract all the images and accumulate them in the acc variable"""
    if isinstance(obj, h5py.Dataset):
        if 'interpretation' in obj.attrs and obj.attrs['interpretation'] == b'image':
            acc.append(obj)
    return None


def visit_key(acc: List[h5py.Dataset], key: str, name: str, obj) -> None:
    if key in name:
        acc.append(obj)
    return None


def get_images(h5file: h5py.File) -> List[h5py.Dataset]:
    acc = []
    h5file.visititems(functools.partial(visit_images, acc))
    return acc


def get_tx(h5file: h5py.File) -> List[h5py.Dataset]:
    txs = []
    h5file.visititems(functools.partial(visit_key, txs, "scan_data/actuator_1_1"))
    return txs


def get_tz(h5file: h5py.File) -> List[h5py.Dataset]:
    tzs = []
    h5file.visititems(functools.partial(visit_key, tzs, "MARS/D03-1-CX0__DT__DTC_2D-MT_Tz__#1/raw_value"))
    return tzs


def visit_item(key: str, name: str, obj: h5py.Dataset) -> h5py.Dataset:
    if key in name:
        return obj


def get_item(h5file: h5py.File, item: H5OptionalItemValue) -> float:
    _item = h5file.visititems(functools.partial(visit_item, item.path))
    return _item.value if _item else item.default


def get_metadata(h5file: h5py.File,
                 h5path: H5OptionalItemValue,
                 index: int) -> MetaData:
    img = get_images(h5file)[0][index]
    tx = get_tx(h5file)[0][index]
    tz = get_item(h5file, h5path)
    return MetaData(img, tx, tz)

def gen_metadata(h5file: h5py.File,
                 h5path: H5OptionalItemValue) -> Iterator[MetaData]:
    imgs = get_images(h5file)[0]
    txs = get_tx(h5file)[0]
    tz = get_item(h5file, h5path)
    for idx in range(imgs.shape[0]):
        yield MetaData(imgs[idx], txs[idx], tz)

def save_as_edf(calib: MultiCalib, basedir: str) -> None:
    """Save the multi calib images into edf files in order to do the first
    calibration

    """
    with h5py.File(calib.filename, mode='r') as h5file:
        imgs = get_images(h5file)[0]
        for idx in calib.idxs:
            img = imgs[idx]
            base = os.path.splitext(os.path.basename(calib.filename))[0]
            output = os.path.join(basedir, base + '_%d.edf' % (idx,))
            edfimage(img).write(output)


def get_total_length(calib: MultiCalib) -> int:
    """Return the total number of frame of the calib file"""
    with h5py.File(calib.filename, mode='r') as f:
        return get_images(f)[0].shape[0]


def optimize_with_new_images(h5file: h5py.File,
                             multicalib: MultiCalib,
                             gonioref,
                             calibrant: pyFAI.calibrant.Calibrant,
                             pts_per_deg: float=1) -> None:
    """This function adds new images to the pool of data used for the
    refinement.  A set of new control points are extractred and a
    refinement step is performed at each iteration The last image of
    the serie is displayed

    """
    sg = None
    for idx in range(get_total_length(multicalib)-1):
        print()
        base = os.path.splitext(os.path.basename(multicalib.filename))[0]

        label = base + "_%d" % (idx,)
        if label in gonioref.single_geometries:
            continue
        print(label)
        metadata = get_metadata(h5file, multicalib.h5path, idx)
        sg = gonioref.new_geometry(label, image=metadata.img, metadata=metadata, calibrant=calibrant)
        print(sg.extract_cp(pts_per_deg=pts_per_deg))
    print("*"*50)
    gonioref.refine2()
    if sg:
        sg.geometry_refinement.set_param(gonioref.get_ai(sg.get_position()).param)
        jupyter.display(sg=sg)

# Extraction de l'image n°5 afin de faire la calibration avec pyFAI-calib2.

# In[5]:

# save this image as edf in order to generate the poni with pyFAI-calib2

def get_wavelength(multicalib: MultiCalib) -> float:
    """Return the wavelength"""
    return multicalib.wavelength


def get_calibrant(multicalib: MultiCalib) -> pyFAI.calibrant.Calibrant:
    """Return the calibrant with the right wavelength"""
    calibrant = pyFAI.calibrant.get_calibrant(multicalib.calibrant)
    calibrant.wavelength = get_wavelength(multicalib)
    return calibrant


def get_detector(multicalib: MultiCalib) -> pyFAI.Detector:
    return pyFAI.detector_factory(multicalib.detector)


def calibration(json: str) -> None:
    """Do a calibration with a bunch of images"""

    wavelength = 4.85945727522e-11
    multicalib = MultiCalib(os.path.join(ROOT, "scan_3_01.nxs"),
                            H5OptionalItemValue("MARS/D03-1-CX0__DT__DTC_2D-MT_Tz__#1/raw_value", 0.0),
                            [2, 5, 8], "LaB6", "xpad_flat", wavelength)

    multicalib2 = MultiCalib(os.path.join(ROOT, "scan_4_01.nxs"),
                             H5OptionalItemValue("MARS/D03-1-CX0__DT__DTC_2D-MT_Tz__#1/raw_value", -1.0),
                             [], "LaB6", "xpad_flat", wavelength)

   # save all the ref as images in order to do the calibration with
    # pyFAI-calib[2].
    save_as_edf(multicalib, PUBLISHED)

    # Definition of the geometry refinement: the parameter order is
    # the same as the param_names
    calibrant = get_calibrant(multicalib)
    detector = get_detector(multicalib)

    distance = 0.258705917299
    poni1_scale = 0.001
    poni1_offset = 0.132825374721
    poni2_scale = 0.0012272727272727272
    poni2_offset = -0.9488181818181818
    rot1 = 0.00388272369359
    rot2 = -0.00942588451226
    rot3 = 7.19961198098e-07

    parameters = [ Parameter("dist", distance, (distance, distance)),
                   Parameter("poni1_offset", poni1_offset, (0, 0.2)),
                   Parameter("poni1_scale", poni1_scale, (0, 0.002)),
                   Parameter("poni2_offset", poni2_offset, (-1, -0.7)),
                   Parameter("poni2_scale", poni2_scale, (-1, 1)),
                   Parameter("rot1", rot1, (rot1, rot1)),
                   Parameter("rot2", rot2, (rot2, rot2)),
                   Parameter("rot3", rot3, (rot3, rot3))
    ]

    params = {p.name: p.value for p in parameters}
    bounds = {p.name: p.bounds for p in parameters}
    param_names = [p.name for p in parameters]

    # Let's refine poni1 and poni2 also as function of the distance:

    trans_function = GeometryTransformation(param_names=param_names,
                                            pos_names = ["tx", "tz"],
                                            dist_expr="dist",
                                            poni1_expr="tz * poni1_scale + poni1_offset",
                                            poni2_expr="tx * poni2_scale + poni2_offset",
                                            rot1_expr="rot1",
                                            rot2_expr="rot2",
                                            rot3_expr="rot3")

    def pos_function(metadata: MetaData) -> Tuple[(float, float)]:
        """Definition of the function reading the detector position from the
        header of the image."""
        return metadata.tx, metadata.tz

    gonioref = GoniometerRefinement(params,  # initial guess
                                    bounds=bounds,
                                    pos_function=pos_function,
                                    trans_function=trans_function,
                                    detector=detector,
                                    wavelength=wavelength)

    print("Empty refinement object:")
    print(gonioref)


    # In[20]:

    # Let's populate the goniometer refinement object with the know poni

    with h5py.File(multicalib.filename, mode='r') as h5file:
        for idx in multicalib.idxs:
            base = os.path.splitext(os.path.basename(multicalib.filename))[0]

            label = base + "_%d" % (idx,)
            metadata = get_metadata(h5file, multicalib.h5path, idx)
            control_points = os.path.join(PUBLISHED, base + "_%d.npt" % (idx,))
            ai = pyFAI.load(os.path.join(PUBLISHED, base + "_%d.poni" % (idx,)))
            print(ai)

            gonioref.new_geometry(label, metadata.img, metadata, control_points, calibrant, ai)

        print("Filled refinement object:")
        print(gonioref)
        print(os.linesep + "\tlabel \t tx")
        for k, v in gonioref.single_geometries.items():
            print(k, v.get_position())

        for g in gonioref.single_geometries.values():
            ai = gonioref.get_ai(g.get_position())
            print(ai)

        for sg in gonioref.single_geometries.values():
            jupyter.display(sg=sg)

        gonioref.refine2()

    for multi in [multicalib, multicalib2]:
        with h5py.File(multi.filename, mode='r') as h5file:
            optimize_with_new_images(h5file, multi, gonioref, calibrant)

    # for idx, sg in enumerate(gonioref.single_geometries.values()):
    #    sg.geometry_refinement.set_param(gonioref.get_ai(sg.get_position()).param)
    #    jupyter.display(sg=sg)

    gonioref.save(json)

    # pylab.show()


def integrate(json: str) -> None:
    """Integrate a file with a json calibration file"""
    filename = os.path.join(ROOT, "scan_77_01.nxs")
    gonio = pyFAI.goniometer.Goniometer.sload(json)
    h5path = H5OptionalItemValue("MARS/D03-1-CX0__DT__DTC_2D-MT_Tz__#1/raw_value", -1.0)

    with h5py.File(filename, mode='r') as h5file:
        images = []
        positions = []
        for metadata in gen_metadata(h5file, h5path):
            images.append(metadata.img)
            positions.append((metadata.tx, metadata.tz))
        mai = gonio.get_mg(positions)
        res = mai.integrate1d(images, 10000)
        jupyter.plot1d(res)
        pylab.show()


if __name__ == "__main__":
    json = os.path.join(PUBLISHED, "calibration.json")
    calibration(json)
    integrate(json)
