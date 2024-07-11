{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Hkl.Geometry
       ( Factory(..)
       , Geometry(..)
       , GeometryState(..)
       , cirpad
       , fixed
       , sixsMedHGisaxs
       , sixsMedVGisaxs
       , sixsUhvGisaxs
       , zaxis
       , withGeometry
       ) where

import           Data.Aeson            (FromJSON (..), ToJSON (..))
import           Data.Tree             (Tree (..), foldTree)
import qualified Data.Vector.Storable  as V
import           Foreign               (ForeignPtr, Ptr, newForeignPtr, nullPtr,
                                        withForeignPtr)
import           Foreign.C             (CDouble (..), newCString, withCString)
import           GHC.Generics          (Generic)
import           Numeric.LinearAlgebra (Vector)

import           Prelude               hiding (max, min)

import           Hkl.C.Hkl
import           Hkl.Lattice
import           Hkl.Orphan            ()

-------------
-- Factory --
-------------

data Factory = K6c | Fixe | Uhv | Mars | MedH | MedV | SoleilSiriusKappa
  deriving (Generic, FromJSON, ToJSON)

instance Show Factory where
  show K6c               = "K6C"
  show Fixe              = undefined
  show Uhv               = "ZAXIS"
  show Mars              = "SOLEIL MARS"
  show MedH              = "SOLEIL SIXS MED1+2"
  show MedV              = "SOLEIL SIXS MED2+3"
  show SoleilSiriusKappa = "SOLEIL SIRIUS KAPPA"

-- factoryFromString :: String -> Factory
-- factoryFromString s
--   | s == "K6C"  = K6c
--   | s == undefined = Fixe
--   | s == "ZAXIS" = Uhv
--   | s == "SOLEIL MARS" = Mars
--   | s == undefined = MedH
--   | s == "SOLEIL SIXS MED2+3" = MedV
--   | s == "SOLEIL SIRIUS KAPPA" = SoleilSiriusKappa
--   | otherwise   = error $ "unknown diffractometer type:" ++ s

newFactory :: Factory -> IO (Ptr C'HklFactory)
newFactory f = Foreign.C.withCString (show f) $ \cname -> c'hkl_factory_get_by_name cname nullPtr

--------------
-- Geometry --
--------------

data Transformation
  = NoTransformation
  | Rotation Double Double Double
  | Translation Double Double Double
  deriving (Generic, FromJSON, Show, ToJSON)

data Axis
  = Axis String Transformation Unit
  deriving (Generic, FromJSON, Show, ToJSON)

data GeometryState
    = GeometryState Double (Vector CDouble)
      deriving (Generic, FromJSON, Show, ToJSON)

data Geometry
  = Geometry'Custom (Tree Axis) (Maybe GeometryState)
  | Geometry'Factory Factory (Maybe GeometryState)
    deriving (Generic, FromJSON, Show, ToJSON)

cirpad :: Geometry
cirpad = Geometry'Custom
    ( Node (Axis "" NoTransformation Unit'NoUnit)
      [ Node (Axis "" NoTransformation Unit'NoUnit) []
      , Node (Axis "delta" (Translation 0 (-1) 0) Unit'Angle'Degree) [Node (Axis "gamma" (Translation 0 0 1) Unit'Angle'Degree) []]
      ]
    )
    Nothing

fixed :: Geometry
fixed
  = Geometry'Custom
    (Node (Axis "mu" (Rotation 0 0 1) Unit'Angle'Degree) [ Node (Axis "omega" (Rotation 0 (-1) 0) Unit'Angle'Degree) [] ])
    Nothing


sixsMedHGisaxs :: Geometry
sixsMedHGisaxs
  = Geometry'Custom
    ( Node (Axis "" NoTransformation Unit'NoUnit)
      [ Node (Axis "beta" (Rotation 0 (-1) 0) Unit'Angle'Degree) [Node (Axis "mu" (Rotation 0 0 1) Unit'Angle'Degree) [] ]
      , Node (Axis "eix" (Translation 0 (-1) 0) Unit'Length'MilliMeter) [Node (Axis "eiz" (Translation 0 0 1) Unit'Length'MilliMeter) []]
      ]
    )
    Nothing

sixsMedVGisaxs :: Geometry
sixsMedVGisaxs
  = Geometry'Custom
    ( Node (Axis "" NoTransformation Unit'NoUnit)
      [ Node (Axis "beta" (Rotation 0 (-1) 0) Unit'Angle'Degree) [Node (Axis "mu" (Rotation 0  0 1) Unit'Angle'Degree) [Node (Axis "omega" (Rotation 0 (-1) 0) Unit'Angle'Degree) [] ] ]
      , Node (Axis "eix" (Translation 0 (-1) 0) Unit'Length'MilliMeter) [Node (Axis "eiz" (Translation 0 0 1) Unit'Length'MilliMeter) []]
      ]
    )
    Nothing

sixsUhvGisaxs :: Geometry
sixsUhvGisaxs
  = Geometry'Custom
    ( Node (Axis "" NoTransformation Unit'NoUnit)
      [ Node (Axis "mu" (Rotation 0 0 1) Unit'Angle'Degree) [ Node (Axis "omega" (Rotation 0 (-1) 0) Unit'Angle'Degree) [] ]
      , Node (Axis "eix" (Translation 0 (-1) 0) Unit'Length'MilliMeter) [Node (Axis "eiz" (Translation 0 0 1) Unit'Length'MilliMeter) []]
      ]
    )
    Nothing

zaxis :: Geometry
zaxis
  = Geometry'Custom
    ( Node (Axis "mu" (Rotation 0 0 1) Unit'Angle'Degree)
      [ Node (Axis "omega" (Rotation 0 (-1) 0) Unit'Angle'Degree) []
      , Node (Axis "delta" (Rotation 0 (-1) 0) Unit'Angle'Degree) [ Node (Axis "gamma" (Rotation 0 0 1) Unit'Angle'Degree) [] ]
      ]
    )
    Nothing

pokeGeometry :: ForeignPtr C'HklGeometry -> GeometryState -> IO ()
pokeGeometry fptr (GeometryState lw vs) =
  withForeignPtr fptr $ \ptr -> do
  -- set the source
  let wavelength = CDouble lw
  c'hkl_geometry_wavelength_set ptr wavelength c'HKL_UNIT_USER nullPtr

  -- set the axes
  let n = toEnum . V.length $ vs
  V.unsafeWith vs $ \values ->
    c'hkl_geometry_axis_values_set ptr values n c'HKL_UNIT_USER nullPtr

newGeometry :: Geometry -> IO (ForeignPtr C'HklGeometry)
newGeometry (Geometry'Custom g ms)
  = do
  gPtr <- c'hkl_geometry_new nullPtr nullPtr
  mapM_ (addHolder gPtr) (axes g)
  fptr <- newForeignPtr p'hkl_geometry_free gPtr
  case ms of
    Nothing  -> return ()
    (Just s) -> pokeGeometry fptr s
  return fptr
    where
      addHolder :: Ptr C'HklGeometry -> [Axis] -> IO ()
      addHolder gPtr axs = do
        hPtr <- c'hkl_geometry_add_holder gPtr
        mapM_ (addAxis hPtr) axs

      addAxis :: Ptr C'HklHolder -> Axis -> IO ()
      addAxis h (Axis n t u) = case t of
        NoTransformation -> return ()
        Rotation x y z -> do
          c'n <- newCString n
          c'hkl_holder_add_rotation h c'n (CDouble x) (CDouble y) (CDouble z) (unitToHklUnit u)
        Translation x y z -> do
          c'n <- newCString n
          c'hkl_holder_add_translation h c'n (CDouble x) (CDouble y) (CDouble z) (unitToHklUnit u)

      axes :: Tree Axis -> [[Axis]]
      axes g' = foldTree (\x xsss -> if null xsss then [[x]] else concatMap (map (x:)) xsss) g'
newGeometry (Geometry'Factory f ms)
    = do fptr <- newForeignPtr p'hkl_geometry_free =<< c'hkl_factory_create_new_geometry =<< newFactory f
         case ms of
           Nothing  -> return ()
           (Just s) -> pokeGeometry fptr s
         return fptr

withGeometry :: Geometry -> (Ptr C'HklGeometry -> IO r) -> IO r
withGeometry g f
    = do fptr <- newGeometry g
         withForeignPtr fptr f
