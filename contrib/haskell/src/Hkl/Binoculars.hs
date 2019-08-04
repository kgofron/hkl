{-
    Copyright  : Copyright (C) 2014-2019 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE QuasiQuotes              #-}
module Hkl.Binoculars
    ( Axis(..)
    , Axes(..)
    , Space(..)
    , axisLength
    , fromImage
    , getIndex
    , processImage
    ) where

import           Data.Array.Repa                 (Array)
import           Data.Array.Repa.Index           (DIM2, DIM3)
import           Data.Array.Repa.Repr.ForeignPtr (F)
import           Data.Text                       (Text)
import           Data.Word                       (Word16)
import           Prelude                         hiding (mapM)

data Axis = Axis
  Int    -- imin     lower bound
  Int    -- imax     upper bound
  Double -- res     step size / resolution
  Text   -- label   human-readable identifier

-- class Axis(object):
--     """Represents a single dimension finite discrete grid centered at 0.

--     Important attributes:
--     min     lower bound
--     max     upper bound
--     res     step size / resolution
--     label   human-readable identifier

--     min, max and res are floats, but internally only integer operations are used. In particular
--     min = imin * res, max = imax * res
--     """

--     def __init__(self, min, max, res, label=None):
--         self.res = float(res)
--         if isinstance(min, int):
--             self.imin = min
--         else:
--             self.imin = int(numpy.floor(min / self.res))
--         if isinstance(max, int):
--             self.imax = max
--         else:
--             self.imax = int(numpy.ceil(max / self.res))
--         self.label = label



getIndex :: Axis -> Array r1 DIM2 Double -> Array r2 DIM2 Int
getIndex _ _ = undefined

    -- def get_index(self, value):
    --     if isinstance(value, numbers.Number):
    --         intvalue = int(round(value / self.res))
    --         if self.imin <= intvalue <= self.imax:
    --             return intvalue - self.imin
    --         raise ValueError('cannot get index: value {0} not in range [{1}, {2}]'.format(value, self.min, self.max))
    --     elif isinstance(value, slice):
    --         if value.step is not None:
    --             raise IndexError('stride not supported')
    --         if value.start is None:
    --             start = None
    --         else:
    --             start = self.get_index(value.start)
    --         if value.stop is None:
    --             stop = None
    --         else:
    --             stop = self.get_index(value.stop)
    --         if start is not None and stop is not None and start > stop:
    --             start, stop = stop, start
    --         return slice(start, stop)
    --     else:
    --         intvalue = numpy.around(value / self.res).astype(int)
    --         if ((self.imin <= intvalue) & (intvalue <= self.imax)).all():
    --             return intvalue - self.imin
    --         raise ValueError('cannot get indices, values from [{0}, {1}], axes range [{2}, {3}]'.format(value.min(), value.max(), self.min, self.max))


axisLength :: Axis -> Int
axisLength (Axis imin imax _ _ ) = imax - imin + 1

    -- def __len__(self):
    --     return self.imax - self.imin + 1



data Axes = Axes [Axis]

-- class Axes(object):
--     """Luxurious tuple of Axis objects."""

--     def __init__(self, axes):
--         self.axes = tuple(axes)
--         if len(self.axes) > 1 and any(axis.label is None for axis in self.axes):
--             raise ValueError('axis label is required for multidimensional space')


data Space = Space
  Axes -- ^  axes - Axes instances describing range and stepsizes of each of the dimensions
  (Array F DIM3 Double) -- ^ photons - n-dimension float array, total intensity per grid point
  (Array F DIM3 Int) -- ^ contributions - n-dimensional numpy integer array, number of original datapoints (pixels) per grid point
  Int -- ^ dimension n

    -- def __init__(self, axes, config=None, metadata=None):
    --     if not isinstance(axes, Axes):
    --         self.axes = Axes(axes)
    --     else:
    --         self.axes = axes

    --     self.config = config
    --     self.metadata = metadata

    --     self.photons = numpy.zeros([len(ax) for ax in self.axes], order='C')
    --     self.contributions = numpy.zeros(self.photons.shape, order='C')

processImage :: Space -> Array F DIM3 Double -> Array F DIM2 Word16 -> Array F DIM2 Bool -> Space
processImage s _coordinates _intensity _weights = s
        -- if len(coordinates) != len(self.axes):
        --     raise ValueError('dimension mismatch between coordinates and axes')

        -- intensity = numpy.nan_to_num(intensity).flatten()  # invalids can be handeled by setting weight to 0, this ensures the weights can do that
        -- weights = weights.flatten()

        -- indices = numpy.array(tuple(ax.get_index(coord) for (ax, coord) in zip(self.axes, coordinates)))
        -- for i in range(0, len(self.axes)):
        --     for j in range(i+1, len(self.axes)):
        --         indices[i, :] *= len(self.axes[j])
        -- indices = indices.sum(axis=0).astype(int).flatten()

        -- photons = numpy.bincount(indices, weights=intensity * weights)
        -- contributions = numpy.bincount(indices, weights=weights)

        -- self.photons.ravel()[:photons.size] += photons
        -- self.contributions.ravel()[:contributions.size] += contributions

fromImage :: [Double] -> [Text] -> [Array F DIM2 Double]  -> Array F DIM2 Word16 -> Array F DIM2 Double -> g -> Space
fromImage _ _ _ _ _ = undefined

    -- @classmethod
    -- def from_image(cls, resolutions, labels, coordinates, intensity, weights, limits=None):
    --     """Create Space from image data.

    --     resolutions   n-tuple of axis resolutions
    --     labels          n-tuple of axis labels
    --     coordinates   n-tuple of data coordinate arrays
    --     intensity     data intensity array"""

    --     if limits is not None:
    --         invalid = numpy.zeros(intensity.shape).astype(numpy.bool)
    --         for coord, sl in zip(coordinates, limits):
    --             if sl.start is None and sl.stop is not None:
    --                 invalid += coord > sl.stop
    --             elif sl.start is not None and sl.stop is None:
    --                 invalid += coord < sl.start
    --             elif sl.start is not None and sl.stop is not None:
    --                 invalid += numpy.bitwise_or(coord < sl.start, coord > sl.stop)

    --         if numpy.all(invalid == True):
    --             return EmptySpace()
    --         coordinates = tuple(coord[~invalid] for coord in coordinates)
    --         intensity = intensity[~invalid]
    --         weights = weights[~invalid]

    --     axes = tuple(Axis(coord.min(), coord.max(), res, label) for res, label, coord in zip(resolutions, labels, coordinates))
    --     newspace = cls(axes)
    --     newspace.process_image(coordinates, intensity, weights)
    --     return newspace
