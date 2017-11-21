#!/usr/bin/env python3
""" coding: utf-8
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

ROOT = "/home/experiences/diffabs/reguer/20170063/"
PUBLISHED = os.path.join(ROOT, "published-data")
CALIB = os.path.join(PUBLISHED, "xrd", "calibrationCeO2")

MetaDataSource = NamedTuple("MetaDataSource", [("image", H5Path),
                                               ("delta", H5Path)])

MetaData = NamedTuple("MetaData", [("image", ndarray),
                                   ("delta", float)])

_Diffabs = NamedTuple("_Diffabs", [("filename", Text),
                                   ("basedir", Text),
                                   ("metasources", MetaDataSource),
                                   ("idxs", List[int]),
                                   ("calibrant", Text),
                                   ("detector", Text),
                                   ("wavelength", float)])


class Diffabs(_Diffabs):
    def __len__(self) -> int:
        with h5py.File(self.filename, mode='r') as f:
            return get_shape(f, self.metasources.image)[0]

    def __item(self, f: h5py.File, index: int) -> MetaData:
        return MetaData(get_item_at_index(f,
                                          self.metasources.image, index),
                        get_item_at_index(f,
                                          self.metasources.delta, index))

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


def save_as_edf(calib: Diffabs,
                basedir: Text) -> None:
    """Save the multi calib images into edf files in order to do the first
    calibration"""
    for idx, metadata in zip(calib.idxs, calib.frames()):
        base = os.path.splitext(os.path.basename(calib.filename))[0]
        output = os.path.join(basedir, base + '_%d.edf' % (idx,))
        edfimage(metadata.spectrum).write(output)


def get_wavelength(multicalib: Diffabs) -> float:
    """Return the wavelength"""
    return multicalib.wavelength


def get_calibrant(multicalib: Diffabs) -> pyFAI.calibrant.Calibrant:
    """Return the calibrant with the right wavelength"""
    calibrant = pyFAI.calibrant.get_calibrant(multicalib.calibrant)
    calibrant.wavelength = get_wavelength(multicalib)
    return calibrant


def get_detector(multicalib: Diffabs) -> pyFAI.Detector:
    return pyFAI.detector_factory(multicalib.detector)


def optimize_with_new_images(multicalib: Diffabs,
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
        label = "CeO2_img%d_poni" % (idx,)
        if label in gonioref.single_geometries:
            continue
        print(label)

        sg = gonioref.new_geometry(label, image=metadata.image,
                                   metadata=metadata, calibrant=calibrant)
        print(sg.extract_cp(pts_per_deg=pts_per_deg))# , "watershed"))
    print("*"*50)
    gonioref.refine2()
    if sg:
        sg.geometry_refinement.set_param(gonioref.get_ai(sg.get_position()).param)  # noqa
        jupyter.display(sg=sg)


def calibration(json: str, multicalib:Diffabs) -> None:
    """Do a calibration with a bunch of images"""

    # Definition of the geometry refinement: the parameter order is
    # the same as the param_names
    calibrant = get_calibrant(multicalib)
    detector = get_detector(multicalib)

    distance = 0.39
    poni1 = -0.01
    poni2 = 0.033
    rot1 = 0
    rot2_scale = -1 * pi / 180.
    rot2_offset = radians(16)
    rot3 = radians(-90)

    parameters = [Parameter("dist", distance, (distance, distance)),
                  Parameter("poni1", poni1, (poni1-1, poni1+1)),
                  Parameter("poni2", poni2, (poni2-1, poni2+1)),
                  Parameter("rot1", rot1, (rot1, rot1)),
                  Parameter("rot2_scale", rot2_scale, (rot2_scale, rot2_scale)),
                  Parameter("rot2_offset", rot2_offset, (rot2_offset-0.1, rot2_offset+0.1)),
                  Parameter("rot3", rot3, (rot3, rot3))]

    params = {p.name: p.value for p in parameters}
    bounds = {p.name: p.bounds for p in parameters}
    param_names = [p.name for p in parameters]

    # Let's refine poni1 and poni2 also as function of the distance:

    trans_function = GeometryTransformation(param_names=param_names,
                                            pos_names=["delta"],
                                            dist_expr="dist",
                                            poni1_expr="poni1",
                                            poni2_expr="poni2",
                                            rot1_expr="rot1",
                                            rot2_expr="delta * rot2_scale + rot2_offset",
                                            rot3_expr="rot3")

    def pos_function(metadata: MetaData) -> Tuple[float]:
        """Definition of the function reading the detector position from the
        header of the image."""
        return metadata.delta,

    gonioref = GoniometerRefinement(params,  # initial guess
                                    bounds=bounds,
                                    pos_function=pos_function,
                                    trans_function=trans_function,
                                    detector=detector,
                                    wavelength=multicalib.wavelength)

    print("Empty refinement object:")
    print(gonioref)

    # Let's populate the goniometer refinement object with the known poni
    for idx, metadata in zip(multicalib.idxs, multicalib.frames()):
        label = "CeO2_img%d_poni" % (idx,)
        control_points = os.path.join(multicalib.basedir, "img%d_pts.npt" % (idx,))
        ai = pyFAI.load(os.path.join(multicalib.basedir, label))  # noqa
        print(ai)

        gonioref.new_geometry(label, metadata.image, metadata,
                              control_points, calibrant, ai)

    print("Filled refinement object:")
    print(gonioref)
    print(os.linesep + "\tlabel \t delta")
    for k, v in gonioref.single_geometries.items():
        print(k, v.get_position())

    for g in gonioref.single_geometries.values():
        ai = gonioref.get_ai(g.get_position())
        print(ai)

    for sg in gonioref.single_geometries.values():
        jupyter.display(sg=sg)

    gonioref.refine2()

    for multi in [multicalib]:
        optimize_with_new_images(multi, gonioref, calibrant)

    for idx, sg in enumerate(gonioref.single_geometries.values()):
        sg.geometry_refinement.set_param(gonioref.get_ai(sg.get_position()).param)
        jupyter.display(sg=sg)

    gonioref.save(json)
    pylab.show()


def integrate(json: Text) -> None:
    """Integrate a file with a json calibration file"""
    gonio = pyFAI.goniometer.Goniometer.sload(json)
    filename = os.path.join(ROOT, "tth2C-16_11_15-55-22_008.nxs")
    wavelength = 0.67186e-10
    multicalib = Diffabs(filename,
                        MetaDataSource(H5PathWithAttribute("interpretation", b"spectrum"),
                                       H5PathContains("scan_data/actuator_1_1")),
                        [], 0, "LaB6", "mythen", wavelength)

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
    json = os.path.join(CALIB, "diffabs.json")
    filename = os.path.join(ROOT, "2017", "Run5", "2017-11-15", "scan_81.nxs")
    wavelength = 6.81198566418e-11
    calib = Diffabs(filename,
                    CALIB,
                    MetaDataSource(H5PathWithAttribute("interpretation", b"image"),
                                   H5PathContains("scan_data/actuator_1_1")),
                    [3, 4], "CeO2", "imxpad_s140", wavelength)

    calibration(json, calib)

if __name__ == "__main__":
    main()
