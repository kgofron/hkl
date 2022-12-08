{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}

module Hkl.Geometry
       ( Factory(..)
       , Geometry(..)
       , Source(..)
       , WaveLength
       , factoryFromString
       , newFactory
       , newGeometry
       , peekHklGeometryList
       , withGeometry
       ) where

import           Prelude                           hiding (max, min)

import           Foreign                           (ForeignPtr, Ptr,
                                                    newForeignPtr, nullPtr,
                                                    withForeignPtr)
import           Foreign.C                         (CDouble (..), withCString)
import           Numeric.LinearAlgebra

import           Control.Monad.Loops               (unfoldrM)
import qualified Data.Vector.Storable              as V
import           Numeric.Units.Dimensional.Prelude (Length, meter, nano, (/~))

import           Hkl.C.Hkl
import           Hkl.Parameter

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
newFactory f = withCString (show f) $ \cname -> c'hkl_factory_get_by_name cname nullPtr

--------------
-- Geometry --
--------------

--  Source

type WaveLength = Length Double

newtype Source = Source WaveLength deriving (Show)

data Geometry = Geometry
                Factory --  the type of diffractometer
                Source --  source
                (Vector CDouble) --  axes position
                (Maybe [Parameter]) --  axes configuration
              deriving (Show)


-- peekAxis :: Ptr C'HklGeometry -> CString -> IO (Ptr C'HklParameter)
-- peekAxis ptr s = c'hkl_geometry_axis_get ptr s nullPtr

-- instance Storable Geometry where
--   alignment _ = alignment (undefined :: Ptr Geometry)

--   sizeOf _ = sizeOf (undefined :: Ptr Geometry)

--   peek ptr = do
--     -- get the HklFactory
--     f_name <- c'hkl_geometry_name_get ptr >>= peekCString
--     let factory = factoryFromString f_name

--     -- Source
--     (CDouble w) <- c'hkl_geometry_wavelength_get ptr unit
--     let source = Source (w *~ nano meter)

--     (DArray n axis_names) <- peek =<< c'hkl_geometry_axis_names_get ptr
--     v <- MV.new (fromEnum n)
--     MV.unsafeWith v $ \values ->
--       c'hkl_geometry_axis_values_get ptr values n unit
--     vs <- V.freeze v

--     ps <- mapM (peekAxis ptr) axis_names

--     return $ Geometry factory source vs (Just ps)

--   poke ptr (Geometry _ (Source lw) vs _) = do
--     -- set the source
--     let wavelength = CDouble (lw /~ nano meter)
--     c_hkl_geometry_wavelength_set ptr wavelength unit nullPtr

--     -- set the axes
--     let n = toEnum . V.length $ vs
--     V.unsafeWith vs $ \values ->
--       c_hkl_geometry_axis_values_set ptr values n unit nullPtr

pokeGeometry :: Ptr C'HklGeometry -> Geometry -> IO ()
pokeGeometry ptr (Geometry _ (Source lw) vs _) = do
  -- set the source
  let wavelength = CDouble (lw /~ nano meter)
  c'hkl_geometry_wavelength_set ptr wavelength unit nullPtr

  -- set the axes
  let n = toEnum . V.length $ vs
  V.unsafeWith vs $ \values ->
    c'hkl_geometry_axis_values_set ptr values n unit nullPtr

withGeometry ::  Geometry -> (Ptr C'HklGeometry -> IO b) -> IO b
withGeometry g fun = do
  fptr <- newGeometry g
  withForeignPtr fptr fun

newGeometry :: Geometry -> IO (ForeignPtr C'HklGeometry)
newGeometry g@(Geometry f _ _ _) = do
  ptr <- c'hkl_factory_create_new_geometry =<< newFactory f
  pokeGeometry ptr g
  newForeignPtr p'hkl_geometry_free ptr


------------------
-- GeometryList --
------------------

-- getSolution0 :: ForeignPtr C'HklGeometryList -> IO (Ptr C'HklGeometry)
-- getSolution0 gl = withForeignPtr gl $ \solutions ->
--                   c'hkl_geometry_list_items_first_get solutions
--                   >>= c'hkl_geometry_list_item_geometry_get

-- buildMatrix' :: Element a => CInt -> CInt -> ((CInt, CInt) -> IO a) -> IO (Matrix a)
-- buildMatrix' rc cc f = do
--    let coordinates' = map (\ ri -> map (\ ci -> (ri, ci)) [0 .. (cc - 1)]) [0 .. (rc - 1)]
--    l <- mapM (mapM f) coordinates'
--    return $ fromLists l


--     -- fromLists $ map (map f)
--     --     $ map (\ ri -> map (\ ci -> (ri, ci)) [0 .. (cc - 1)]) [0 .. (rc - 1)]

-- geometryDetectorRotationGet :: Geometry -> Detector a sh -> IO (Matrix Double)
-- geometryDetectorRotationGet g d  = do
--   f_geometry <- newGeometry g
--   f_detector <- newDetector d
--   withForeignPtr f_detector $ \detector ->
--     withForeignPtr f_geometry $ \geometry -> do
--       f_q <- newForeignPtr p'hkl_quaternion_free =<< c'hkl_geometry_detector_rotation_get_binding geometry detector
--       withForeignPtr f_q $ \quaternion -> do
--         f_m <- newForeignPtr p'hkl_matrix_free =<< c'hkl_quaternion_to_matrix_binding quaternion
--         withForeignPtr f_m $ \matrix' ->
--           buildMatrix' 3 3 (getV matrix')
--           where
--             getV :: Ptr C'HklMatrix -> (CInt, CInt) -> IO Double
--             getV m (i', j') = do
--               (CDouble v) <- c'hkl_matrix_get m i' j'
--               return v

peekItems :: Ptr C'HklGeometryList -> IO [Ptr C'HklGeometryListItem]
peekItems l = c'hkl_geometry_list_items_first_get l >>= unfoldrM go
   where
      go e
         | e == nullPtr = return Nothing
         | otherwise    = do
               next <- c'hkl_geometry_list_items_next_get l e
               return (Just (e, next))

peekHklGeometryList :: ForeignPtr C'HklGeometryList -> IO [Ptr C'HklGeometry]
peekHklGeometryList l = withForeignPtr l $ \ls -> do
  items <- peekItems ls
  mapM extract items
    where
      extract it = c'hkl_geometry_list_item_geometry_get it
