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
from math import pi, radians

import h5py
import pylab
import pyFAI

from fabio.edfimage import edfimage
from numpy import ndarray

from pyFAI.goniometer import GeometryTransformation, GoniometerRefinement
from pyFAI.gui import jupyter

from common import *

ROOT = "/home/akira/Downloads"
PUBLISHED = os.path.join(ROOT, "published-data")

CALIB = os.path.join(ROOT, "scan_3_01.nxs")

MetaDataSource = NamedTuple("MetaDataSource", [("spectrum", H5Path),
                                               ("tth", H5Path)])

MetaData = NamedTuple("MetaData", [("spectrum", ndarray),
                                   ("tth", float)])

_Mythen = NamedTuple("_Mythen", [("filename", Text),
                                 ("metasources", MetaDataSource),
                                 ("idxs", List[int]),
                                 ("module", int),
                                 ("calibrant", Text),
                                 ("detector", Text),
                                 ("wavelength", float)])

Parameter = NamedTuple("Parameter", [("name", Text),
                                     ("value", float),
                                     ("bounds", Tuple[float, float])])


def extract_module(spectrum: ndarray, module: int) -> ndarray:
    return spectrum[slice(module*1280, (module+1)*1280, 1)]


class Mythen(_Mythen):
    def __len__(self) -> int:
        with h5py.File(self.filename, mode='r') as f:
            return get_shape(f, self.metasources.spectrum)[0]

    def __item(self, f: h5py.File, index: int) -> MetaData:
        spectrum = extract_module(get_item_at_index(f, self.metasources.spectrum, index), self.module)
        spectrum.shape = (1, spectrum.shape[0])
        return MetaData(spectrum,
                        get_item_at_index(f,
                                          self.metasources.tth, index))

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


def save_as_edf(calib: Mythen,
                basedir: Text) -> None:
    """Save the multi calib images into edf files in order to do the first
    calibration"""
    for idx, metadata in zip(calib.idxs, calib.frames()):
        base = os.path.splitext(os.path.basename(calib.filename))[0]
        output = os.path.join(basedir, base + '_%d.edf' % (idx,))
        edfimage(metadata.spectrum).write(output)


def get_wavelength(multicalib: Mythen) -> float:
    """Return the wavelength"""
    return multicalib.wavelength


def get_calibrant(multicalib: Mythen) -> pyFAI.calibrant.Calibrant:
    """Return the calibrant with the right wavelength"""
    calibrant = pyFAI.calibrant.get_calibrant(multicalib.calibrant)
    calibrant.wavelength = get_wavelength(multicalib)
    return calibrant


def get_detector(multicalib: Mythen) -> pyFAI.Detector:
    return pyFAI.detector_factory(multicalib.detector)


def optimize_with_new_images(multicalib: Mythen,
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

        sg = gonioref.new_geometry(label, image=metadata.spectrum,
                                   metadata=metadata, calibrant=calibrant)
        print(sg.extract_cp(method="blob", pts_per_deg=pts_per_deg))# , "watershed"))
    print("*"*50)
    gonioref.refine2()
    if sg:
        sg.geometry_refinement.set_param(gonioref.get_ai(sg.get_position()).param)  # noqa
        jupyter.display(sg=sg)

def calibration(json: str) -> None:
    """Do a calibration with a bunch of images"""

    filename = os.path.join(ROOT, "tth2C-16_11_15-55-22_008.nxs")
    wavelength = 0.67186e-10
    multicalib = Mythen(filename,
                        MetaDataSource(H5PathWithAttribute("interpretation", b"spectrum"),
                                       H5PathContains("scan_data/actuator_1_1")),
                        [18, 21], 0, "LaB6", "mythen", wavelength)

    save_as_edf(multicalib, PUBLISHED)

    # Definition of the geometry refinement: the parameter order is
    # the same as the param_names
    calibrant = get_calibrant(multicalib)
    detector = get_detector(multicalib)

    distance = 0.617400891837
    poni1 = 0
    poni2 = 0.0323014291288
    rot1_scale = -1 * pi / 180.
    rot1_offset = radians(74.88)
    rot2 = 0
    rot3 = 0

    parameters = [Parameter("dist", distance, (distance, distance)),
                  Parameter("poni1", poni1, (poni1, poni1)),
                  Parameter("poni2", poni2, (poni2, poni2)),
                  Parameter("rot1_offset", rot1_offset, (rot1_offset, rot1_offset)),
                  Parameter("rot1_scale", rot1_scale, (rot1_scale, rot1_scale)),
                  Parameter("rot2", rot2, (rot2, rot2)),
                  Parameter("rot3", rot3, (rot3, rot3))]

    params = {p.name: p.value for p in parameters}
    bounds = {p.name: p.bounds for p in parameters}
    param_names = [p.name for p in parameters]

    # Let's refine poni1 and poni2 also as function of the distance:

    trans_function = GeometryTransformation(param_names=param_names,
                                            pos_names=["tth"],
                                            dist_expr="dist",
                                            poni1_expr="poni1",
                                            poni2_expr="poni2",
                                            rot1_expr="tth * rot1_scale + rot1_offset",
                                            rot2_expr="rot2",
                                            rot3_expr="rot3")

    def pos_function(metadata: MetaData) -> Tuple[float]:
        """Definition of the function reading the detector position from the
        header of the image."""
        return metadata.tth,

    gonioref = GoniometerRefinement(params,  # initial guess
                                    bounds=bounds,
                                    pos_function=pos_function,
                                    trans_function=trans_function,
                                    detector=detector,
                                    wavelength=wavelength)

    print("Empty refinement object:")
    print(gonioref)

    # Let's populate the goniometer refinement object with the known poni
    for idx, metadata in zip(multicalib.idxs, multicalib.frames()):
        base = os.path.splitext(os.path.basename(multicalib.filename))[0]

        label = base + "_%d" % (idx,)
        control_points = os.path.join(PUBLISHED, base + "_%d.npt" % (idx,))
        ai = pyFAI.load(os.path.join(PUBLISHED, base + "_%d.poni" % (idx,)))  # noqa
        print(ai)

        gonioref.new_geometry(label, metadata.spectrum, metadata,
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

    # for multi in [multicalib]:
    #      optimize_with_new_images(multi, gonioref, calibrant)

    # for idx, sg in enumerate(gonioref.single_geometries.values()):
    #      sg.geometry_refinement.set_param(gonioref.get_ai(sg.get_position()).param)
    #      jupyter.display(sg=sg)

    gonioref.save(json)

    # pylab.show()


def integrate(json: Text) -> None:
    """Integrate a file with a json calibration file"""
    gonio = pyFAI.goniometer.Goniometer.sload(json)
    filename = os.path.join(ROOT, "tth2C-16_11_15-55-22_008.nxs")
    wavelength = 0.67186e-10
    multicalib = Mythen(filename,
                        MetaDataSource(H5PathWithAttribute("interpretation", b"spectrum"),
                                       H5PathContains("scan_data/actuator_1_1")),
                        [18, 21], 0, "LaB6", "mythen", wavelength)

    # print(len(multicalib))
    # metadata = multicalib.__item__(21)
    
    # ai = gonio.get_ai(metadata.tth)
    # res = ai.integrate1d(metadata.spectrum, 1280, unit="2th_deg")
    # jupyter.plot1d(res)

    images = []
    positions = []
    for metadata in multicalib.all_frames():
        images.append(metadata.spectrum)
        positions.append((metadata.tth,))
    mai = gonio.get_mg(positions)
    res = mai.integrate1d(images, 30000)
    jupyter.plot1d(res)
    pylab.show()


def main() -> None:
    filename = os.path.join(ROOT, "tth2C-16_11_15-55-22_008.nxs")
    wavelength = 0.67186e-10
    mythen = Mythen(filename,
                    MetaDataSource(H5PathWithAttribute("interpretation", b"spectrum"),
                                   H5PathContains("scan_data/actuator_1_1")),
                    [18, 21], 0, "LaB6", "mythen", wavelength)

    save_as_edf(mythen, PUBLISHED)
    
    for frame in mythen.all_frames():
        print(frame)
    
if __name__ == "__main__":
    json = os.path.join(PUBLISHED, "mythen1.json")
    calibration(json)
    integrate(json)
