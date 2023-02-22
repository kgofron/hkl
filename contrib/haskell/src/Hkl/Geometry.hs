{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Hkl.Geometry
       ( Factory(..)
       , Geometry(..)
       , factoryFromString
       , fixed
       , newFactory
       , newGeometry
       , pokeGeometry
       , zaxis
       ) where

import           Data.Text             (Text)
import           Data.Tree             (Tree (..), foldTree)
import qualified Data.Vector.Storable  as V
import           Foreign               (ForeignPtr, Ptr, newForeignPtr, nullPtr,
                                        withForeignPtr)
import           Foreign.C             (CDouble (..), withCString)
import           Numeric.LinearAlgebra (Vector)

import           Prelude               hiding (max, min)

import           Hkl.C.Hkl
import           Hkl.Parameter
import           Hkl.Utils

-------------
-- Factory --
-------------

data Factory = K6c | Fixe | Uhv | Mars | MedH | MedV | SoleilSiriusKappa

instance Show Factory where
  show K6c               = "K6C"
  show Fixe              = undefined
  show Uhv               = "ZAXIS"
  show Mars              = "SOLEIL MARS"
  show MedH              = "SOLEIL SIXS MED1+2"
  show MedV              = "SOLEIL SIXS MED2+3"
  show SoleilSiriusKappa = "SOLEIL SIRIUS KAPPA"

factoryFromString :: String -> Factory
factoryFromString s
  | s == "K6C"  = K6c
  | s == undefined = Fixe
  | s == "ZAXIS" = Uhv
  | s == "SOLEIL MARS" = Mars
  | s == undefined = MedH
  | s == "SOLEIL SIXS MED2+3" = MedV
  | s == "SOLEIL SIRIUS KAPPA" = SoleilSiriusKappa
  | otherwise   = error $ "unknown diffractometer type:" ++ s

newFactory :: Factory -> IO (Ptr C'HklFactory)
newFactory f = Foreign.C.withCString (show f) $ \cname -> c'hkl_factory_get_by_name cname nullPtr

--------------
-- Geometry --
--------------

data Transformation
  = NoTransformation
  | Rotation Double Double Double
  | Translation Double Double Double
  deriving (Show)

data Axis
  = Axis Text Transformation
  deriving (Show)

data Geometry
  = Geometry'Custom (Tree Axis)
  | Geometry'Factory Factory
    deriving (Show)

fixed :: Geometry
fixed
  = Geometry'Custom
    ( Node
      (Axis "mu" (Rotation 0 0 1))
      [ Node
        (Axis "omega" (Rotation 0 (-1) 0))
        []
      ]
    )

zaxis :: Geometry
zaxis
  = Geometry'Custom
    ( Node
      (Axis "mu" (Rotation 0 0 1))
      [ Node
        (Axis "omega" (Rotation 0 (-1) 0))
        []
      , Node
        (Axis "delta" (Rotation 0 (-1) 0))
        [ Node
          (Axis "gamma" (Rotation 0 0 1))
          []
        ]
      ])

newGeometry :: Geometry -> IO (ForeignPtr C'HklGeometry)
newGeometry (Geometry'Custom g)
  = do
  let factoryPtr = nullPtr
  gPtr <- c'hkl_geometry_new factoryPtr p'hkl_geometry_operations_defaults
  mapM_ (addHolder gPtr) (axes g)
  newForeignPtr p'hkl_geometry_free gPtr
    where
      addHolder :: Ptr C'HklGeometry -> [Axis] -> IO ()
      addHolder gPtr axs = do
        hPtr <- c'hkl_geometry_add_holder gPtr
        mapM_ (addAxis hPtr) axs

      addAxis :: Ptr C'HklHolder -> Axis -> IO ()
      addAxis h (Axis n t) = Hkl.Utils.withCString n $ \c'n -> case t of
        NoTransformation -> return ()
        Rotation x y z -> c'hkl_holder_add_rotation h c'n (CDouble x) (CDouble y) (CDouble z) p'hkl_unit_angle_deg
        Translation x y z -> c'hkl_holder_add_translation h c'n (CDouble x) (CDouble y) (CDouble z) p'hkl_unit_length_nm

      axes :: Tree Axis -> [[Axis]]
      axes g' = foldTree (\x xsss -> if null xsss then [[x]] else concatMap (map (x:)) xsss) g'
newGeometry (Geometry'Factory f) = newForeignPtr p'hkl_geometry_free =<< c'hkl_factory_create_new_geometry =<< newFactory f

pokeGeometry :: ForeignPtr C'HklGeometry -> Double -> Vector CDouble -> IO ()
pokeGeometry fptr lw vs =
  withForeignPtr fptr $ \ptr -> do
  -- set the source
  let wavelength = CDouble lw
  c'hkl_geometry_wavelength_set ptr wavelength unit nullPtr

  -- set the axes
  let n = toEnum . V.length $ vs
  V.unsafeWith vs $ \values ->
    c'hkl_geometry_axis_values_set ptr values n unit nullPtr
