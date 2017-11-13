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

# # extraction des scans avec 6 positions en tx, quelque soit tz.
# # LONG
# import glob

# files = glob.glob(os.path.join(ROOT, "*.nxs"))

# def is_ok(filename: str) -> bool:
#     with h5py.File(filename, mode='r') as f:
#         for imgs, tx, tz in zip(get_images(f), get_tx(f), get_tz(f)):
#             return True if tx.shape[0] == 6 else False

# good = [f for f in files if is_ok(f)]
# print(good)
"""

from typing import Iterator, Tuple

import os

from collections import namedtuple
from functools import partial

import h5py
import pylab
import pyFAI

from fabio.edfimage import edfimage

from pyFAI.goniometer import GeometryTransformation, GoniometerRefinement
from pyFAI.gui import jupyter


ROOT = "/home/experiences/instrumentation/picca/jupyter/mars/20160800/"
PUBLISHED = os.path.join(ROOT, "published-data")

CALIB = os.path.join(ROOT, "scan_3_01.nxs")

# H5Path data
H5PathContains = namedtuple("H5PathContains", "path")
H5PathOptionalItemValue = namedtuple('H5OptionalItemValue',
                                     ['path', 'default'])
H5PathWithAttribute = namedtuple("H5PathWithAttribute", ['attribute', 'value'])

MetaDataSource = namedtuple("MetaDataSource", ["images",  # H5Path
                                               "tx",  # H5Path
                                               "tz"])  # H5Path

MetaData = namedtuple("MetaData", ["image", "tx", "tz"])

MultiCalib = namedtuple("MultiCalib", ["filename", "metasources", "idxs",
                                       "calibrant", "detector", "wavelength"])

Parameter = namedtuple("Parameter", ["name", "value", "bounds"])


def _v_attrs(attribute: str, value: str, _name: str, obj) -> None:
    """extract all the images and accumulate them in the acc variable"""
    if isinstance(obj, h5py.Dataset):
        if attribute in obj.attrs and obj.attrs[attribute] == value:
            return obj


def _v_item(key: str, name: str, obj: h5py.Dataset) -> h5py.Dataset:
    if key in name:
        return obj


def get_item(h5file: h5py.File, item):
    res = None
    if isinstance(item, H5PathContains):
        res = h5file.visititems(partial(_v_item, item.path))
    elif isinstance(item, H5PathOptionalItemValue):
        _item = h5file.visititems(partial(_v_item, item.path))
        res = _item.value if _item else item.default
    elif isinstance(item, H5PathWithAttribute):
        res = h5file.visititems(partial(_v_attrs, item.attribute, item.value))
    return res


def get_metadata(h5file: h5py.File,
                 multicalib: MultiCalib,
                 index: int) -> MetaData:
    return MetaData(get_item(h5file, multicalib.metasources.images)[index],
                    get_item(h5file, multicalib.metasources.tx)[index],
                    get_item(h5file, multicalib.metasources.tz))


def gen_metadata(h5file: h5py.File,
                 datasource: MetaDataSource) -> Iterator[MetaData]:
    imgs = get_item(h5file, datasource.images)
    for idx in range(imgs.shape[0]):
        yield MetaData(imgs[idx],
                       get_item(h5file, datasource.tx)[idx],
                       get_item(h5file, datasource.tz))


def save_as_edf(calib: MultiCalib, basedir: str) -> None:
    """Save the multi calib images into edf files in order to do the first
    calibration

    """
    with h5py.File(calib.filename, mode='r') as h5file:
        for idx in calib.idxs:
            metadata = get_metadata(h5file, calib, idx)
            img = metadata.image
            base = os.path.splitext(os.path.basename(calib.filename))[0]
            output = os.path.join(basedir, base + '_%d.edf' % (idx,))
            edfimage(img).write(output)


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
    for idx, metadata in enumerate(gen_metadata(h5file,
                                                multicalib.metasources)):
        print()
        base = os.path.splitext(os.path.basename(multicalib.filename))[0]

        label = base + "_%d" % (idx,)
        if label in gonioref.single_geometries:
            continue
        print(label)

        sg = gonioref.new_geometry(label, image=metadata.image,
                                   metadata=metadata, calibrant=calibrant)
        print(sg.extract_cp(pts_per_deg=pts_per_deg))
    print("*"*50)
    gonioref.refine2()
    if sg:
        sg.geometry_refinement.set_param(gonioref.get_ai(sg.get_position()).param)  # noqa
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
                            MetaDataSource(H5PathWithAttribute("interpretation", b"image"),  # noqa
                                           H5PathContains("scan_data/actuator_1_1"),  # noqa
                                           H5PathOptionalItemValue("MARS/D03-1-CX0__DT__DTC_2D-MT_Tz__#1/raw_value", 0.0)),  # noqa
                            [2, 5, 8], "LaB6", "xpad_flat", wavelength)

    multicalib2 = MultiCalib(os.path.join(ROOT, "scan_4_01.nxs"),
                             MetaDataSource(H5PathWithAttribute("interpretation", b"image"),  # noqa
                                            H5PathContains("scan_data/actuator_1_1"),  # noqa
                                            H5PathOptionalItemValue("MARS/D03-1-CX0__DT__DTC_2D-MT_Tz__#1/raw_value", -1.0)),  # noqa
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

    parameters = [Parameter("dist", distance, (distance, distance)),
                  Parameter("poni1_offset", poni1_offset, (0, 0.2)),
                  Parameter("poni1_scale", poni1_scale, (0, 0.002)),
                  Parameter("poni2_offset", poni2_offset, (-1, -0.7)),
                  Parameter("poni2_scale", poni2_scale, (-1, 1)),
                  Parameter("rot1", rot1, (rot1, rot1)),
                  Parameter("rot2", rot2, (rot2, rot2)),
                  Parameter("rot3", rot3, (rot3, rot3))]

    params = {p.name: p.value for p in parameters}
    bounds = {p.name: p.bounds for p in parameters}
    param_names = [p.name for p in parameters]

    # Let's refine poni1 and poni2 also as function of the distance:

    trans_function = GeometryTransformation(param_names=param_names,
                                            pos_names=["tx", "tz"],
                                            dist_expr="dist",
                                            poni1_expr="tz * poni1_scale + poni1_offset",  # noqa
                                            poni2_expr="tx * poni2_scale + poni2_offset",  # noqa
                                            rot1_expr="rot1",
                                            rot2_expr="rot2",
                                            rot3_expr="rot3")

    def pos_function(metadata: MetaData) -> Tuple[float, float]:
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

    # Let's populate the goniometer refinement object with the known poni

    with h5py.File(multicalib.filename, mode='r') as h5file:
        for idx in multicalib.idxs:
            base = os.path.splitext(os.path.basename(multicalib.filename))[0]

            label = base + "_%d" % (idx,)
            metadata = get_metadata(h5file, multicalib, idx)
            control_points = os.path.join(PUBLISHED, base + "_%d.npt" % (idx,))
            ai = pyFAI.load(os.path.join(PUBLISHED, base + "_%d.poni" % (idx,)))  # noqa
            print(ai)

            gonioref.new_geometry(label, metadata.image, metadata,
                                  control_points, calibrant, ai)

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
    wavelength = 4.85945727522e-11
    multicalib = MultiCalib(os.path.join(ROOT, "scan_4_01.nxs"),
                            MetaDataSource(H5PathWithAttribute("interpretation", b"image"),  # noqa"
                                           H5PathContains("scan_data/actuator_1_1"),  # noqa
                                           H5PathOptionalItemValue("MARS/D03-1-CX0__DT__DTC_2D-MT_Tz__#1/raw_value", -1.0)),  # noqa
                            [], "LaB6", "xpad_flat", wavelength)

    with h5py.File(filename, mode='r') as h5file:
        images = []
        positions = []
        for metadata in gen_metadata(h5file, multicalib.metasources):
            images.append(metadata.image)
            positions.append((metadata.tx, metadata.tz))
        mai = gonio.get_mg(positions)
        res = mai.integrate1d(images, 10000)
        jupyter.plot1d(res)
        pylab.show()


if __name__ == "__main__":
    JSON = os.path.join(PUBLISHED, "calibration.json")
    calibration(JSON)
    integrate(JSON)
