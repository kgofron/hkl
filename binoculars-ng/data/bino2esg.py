#!/usr/bin/python3

import sys

import numpy as np

from h5py import File
from pathlib import Path

def write_esg1(fname, radial, intensities, azimuthal, sigmas=None):
    """Write MAUD esg formatted histogram data to single file."""
    Path(fname).unlink(missing_ok=True)
    with open(fname, 'a+') as f:
        if sigmas is None:
            sigmas = np.zeros_like(intensities)
        for i, (intensity, azimuth, sigma) in enumerate(zip(intensities, azimuthal, sigmas)):
            imask = np.ma.masked_invalid(intensity).mask
            intensity_masked = np.ma.masked_array(intensity, imask).compressed()
            if len(intensity_masked) > 0:
                radial_masked = np.ma.masked_array(radial, imask).compressed()
                sigma_masked = np.ma.masked_array(sigma, imask).compressed()
                header = \
                    f"\n_pd_block_id noTitle|#{i}\n" \
                    f"_pd_meas_angle_eta {azimuth}\n" \
                    f"_pd_meas_angle_omega {0.0:9.4f}\n\n" \
                    f"loop_\n" \
                    f"_pd_meas_position_x _pd_meas_intensity_total _pd_proc_intensity_weight"
                np.savetxt(f,
                           np.c_[radial_masked, intensity_masked, sigma_masked],
                           delimiter='\t',
                           header=header,
                           comments='')

def axis_as_array(axis):
    return np.arange(axis[1], axis[2]+axis[3], axis[3])

def convert(fname, output):
    with File(fname) as f:
        counts = np.sum(f["binoculars/counts"], axis=0)
        contributions = np.sum(f["binoculars/contributions"], axis=0)
        radial = axis_as_array(f["binoculars/axes/tth"][:])
        azimuthal = axis_as_array(f["binoculars/axes/sampleaxis"][:])
        intensities = counts / contributions
        write_esg1(output, radial, intensities, azimuthal)

def main():
    fname = Path(sys.argv[1])
    output = fname.with_suffix(".esg")
    convert(fname, output)

if __name__ == "__main__":
    main()
