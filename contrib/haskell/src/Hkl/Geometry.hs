{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Hkl.Geometry
       ( Factory(..)
       , Geometry(..)
       , Geometry'(..)
       , factoryFromString
       , newFactory
       , newGeometry
       , newGeometry'
       , peekHklGeometryList
       , withGeometry
       , zaxis
       ) where

import           Data.Text             (Text)
import           Data.Tree             (Tree (..), foldTree)
import           Foreign               (ForeignPtr, Ptr, newForeignPtr, nullPtr,
                                        withForeignPtr)
import           Foreign.C             (CDouble (..), withCString)
import           Numeric.LinearAlgebra (Vector)

import           Control.Monad.Loops   (unfoldrM)
import qualified Data.Vector.Storable  as V

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

--  Source

data Geometry = Geometry
                Factory --  the type of diffractometer
                Double --  source
                (Vector CDouble) --  axes position
                (Maybe [Parameter]) --  axes configuration
              deriving (Show)


data Transformation
  = NoTransformation
  | Rotation Double Double Double
  | Translation Double Double Double
  deriving (Show)

data Axis
  = Axis Text Transformation
  deriving (Show)

newtype Geometry'
  = Geometry'
    (Tree Axis)
    deriving (Show)

zaxis :: Geometry'
zaxis
  =  Geometry'
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

newGeometry' :: Geometry' -> IO (ForeignPtr C'HklGeometry)
newGeometry' (Geometry' g)
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

-- static HklGeometry *hkl_geometry_new_zaxis(const HklFactory *factory)
-- {
-- 	HklGeometry *self = hkl_geometry_new(factory, &hkl_geometry_operations_defaults);
-- 	HklHolder *h;

-- 	h = hkl_geometry_add_holder(self);
-- 	hkl_holder_add_rotation(h, MU, 0, 0, 1, &hkl_unit_angle_deg);
-- 	hkl_holder_add_rotation(h, OMEGA, 0, -1, 0, &hkl_unit_angle_deg);

-- 	h = hkl_geometry_add_holder(self);
-- 	hkl_holder_add_rotation(h, MU, 0, 0, 1, &hkl_unit_angle_deg);
-- 	hkl_holder_add_rotation(h, DELTA, 0, -1, 0, &hkl_unit_angle_deg);
-- 	hkl_holder_add_rotation(h, GAMMA, 0, 0, 1, &hkl_unit_angle_deg);

-- 	return self;
-- }

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
pokeGeometry ptr (Geometry _ lw vs _) = do
  -- set the source
  let wavelength = CDouble lw
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
      extract = c'hkl_geometry_list_item_geometry_get
