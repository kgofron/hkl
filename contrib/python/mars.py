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

from typing import Iterator, List, NamedTuple, Text, Tuple, Union

import os

from functools import partial

import h5py
import pylab
import pyFAI

from fabio.edfimage import edfimage
from numpy import ndarray

from pyFAI.goniometer import GeometryTransformation, GoniometerRefinement
from pyFAI.gui import jupyter


ROOT = "/home/experiences/instrumentation/picca/jupyter/mars/20160800/"
PUBLISHED = os.path.join(ROOT, "published-data")

CALIB = os.path.join(ROOT, "scan_3_01.nxs")

# H5Path data constructors
H5PathContains = NamedTuple("H5PathContains", [("path", Text)])

H5PathOptionalItemValue = NamedTuple('H5OptionalItemValue', [('path', Text),
                                                             ('default', float)])  # noqa
H5PathWithAttribute = NamedTuple("H5PathWithAttribute", [('attribute', Text),
                                                         ('value', bytes)])

H5Path = Union[H5PathContains, H5PathOptionalItemValue, H5PathWithAttribute]


MetaDataSource = NamedTuple("MetaDataSource", [("images", H5Path),
                                               ("tx", H5Path),
                                               ("tz", H5Path)])

MetaData = NamedTuple("MetaData", [("image", ndarray),
                                   ("tx", float),
                                   ("tz", float)])

_MultiCalib = NamedTuple("_MultiCalib", [("filename", Text),
                                         ("metasources", MetaDataSource),
                                         ("idxs", List[int]),
                                         ("calibrant", Text),
                                         ("detector", Text),
                                         ("wavelength", float)])

Parameter = NamedTuple("Parameter", [("name", Text),
                                     ("value", float),
                                     ("bounds", Tuple[float, float])])


def _v_attrs(attribute: Text, value: Text, _name: Text, obj) -> h5py.Dataset:
    """extract all the images and accumulate them in the acc variable"""
    if isinstance(obj, h5py.Dataset):
        if attribute in obj.attrs and obj.attrs[attribute] == value:
            return obj


def _v_item(key: Text, name: Text, obj: h5py.Dataset) -> h5py.Dataset:
    if key in name:
        return obj


def get_shape(h5file: h5py.File,
              item: H5Path) -> Tuple:
    res = None
    if isinstance(item, H5PathContains):
        res = h5file.visititems(partial(_v_item, item.path)).shape
    elif isinstance(item, H5PathOptionalItemValue):
        _item = h5file.visititems(partial(_v_item, item.path))
        res = _item.shape if _item else (1,)
    elif isinstance(item, H5PathWithAttribute):
        res = h5file.visititems(partial(_v_attrs,
                                        item.attribute, item.value)).shape
    return res


def get_item_at_index(h5file: h5py.File,
                      item: H5Path,
                      index: int) -> Union[float, ndarray]:
    res = None
    if isinstance(item, H5PathContains):
        res = h5file.visititems(partial(_v_item, item.path))[index]
    elif isinstance(item, H5PathOptionalItemValue):
        _item = h5file.visititems(partial(_v_item, item.path))
        res = _item.value if _item else item.default
    elif isinstance(item, H5PathWithAttribute):
        res = h5file.visititems(partial(_v_attrs,
                                        item.attribute, item.value))[index]
    return res


class MultiCalib(_MultiCalib):
    def __len__(self) -> int:
        with h5py.File(self.filename, mode='r') as f:
            return get_shape(f, self.metasources.images)[0]

    def __item(self, f: h5py.File, index: int) -> MetaData:
        return MetaData(get_item_at_index(f,
                                          self.metasources.images, index),
                        get_item_at_index(f,
                                          self.metasources.tx, index),
                        get_item_at_index(f,
                                          self.metasources.tz, index))

    def __item__(self, index: int) -> MetaData:
        with h5py.File(self.filename, mode='r') as f:
            return self.__item(f, index)

    def frames(self) -> Iterator[MetaData]:
        with h5py.File(self.filename, mode='r') as f:
            for index in self.idxs:
                yield self.__item(f, index)

    def all_frames(self) -> Iterator[MetaData]:
        with h5py.File(self.filename, mode='r') as f:
            for index in range(len(self)):
                yield self.__item(f, index)


def save_as_edf(calib: MultiCalib,
                basedir: Text) -> None:
    """Save the multi calib images into edf files in order to do the first
    calibration"""
    for idx, metadata in enumerate(calib.frames()):
        base = os.path.splitext(os.path.basename(calib.filename))[0]
        output = os.path.join(basedir, base + '_%d.edf' % (idx,))
        edfimage(metadata.image).write(output)


def optimize_with_new_images(multicalib: MultiCalib,
                             gonioref: GoniometerRefinement,
                             calibrant: pyFAI.calibrant.Calibrant,
                             pts_per_deg: float=1) -> None:
    """This function adds new images to the pool of data used for the
    refinement.  A set of new control points are extractred and a
    refinement step is performed at each iteration The last image of
    the serie is displayed

    """
    sg = None
    for idx, metadata in enumerate(multicalib.all_frames()):
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
    for idx, metadata in enumerate(multicalib.frames()):
        base = os.path.splitext(os.path.basename(multicalib.filename))[0]

        label = base + "_%d" % (idx,)
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
        optimize_with_new_images(multi, gonioref, calibrant)

    # for idx, sg in enumerate(gonioref.single_geometries.values()):
    #    sg.geometry_refinement.set_param(gonioref.get_ai(sg.get_position()).param)
    #    jupyter.display(sg=sg)

    gonioref.save(json)

    # pylab.show()


def integrate(json: Text) -> None:
    """Integrate a file with a json calibration file"""
    gonio = pyFAI.goniometer.Goniometer.sload(json)
    wavelength = 4.85945727522e-11
    multicalib = MultiCalib(os.path.join(ROOT, "scan_77_01.nxs"),
                            MetaDataSource(H5PathWithAttribute("interpretation", b"image"),  # noqa"
                                           H5PathContains("scan_data/actuator_1_1"),  # noqa
                                           H5PathOptionalItemValue("MARS/D03-1-CX0__DT__DTC_2D-MT_Tz__#1/raw_value", -1.0)),  # noqa
                            [], "LaB6", "xpad_flat", wavelength)

    images = []
    positions = []
    for metadata in multicalib.all_frames():
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
