module Hkl.Engine
       ( Engine(..)
       , Mode(..)
       , enginesTrajectoryPipe
       , fromToPipe
       , engineName
       , peekEngine
       ) where

import           Control.Monad         (forM_, forever)
import           Foreign               (Ptr, nullPtr)
import           Foreign.C             (CString, peekCString)
import           Numeric.LinearAlgebra (Vector, toList)
import           Pipes                 (Pipe, Producer, await, yield)

import           Hkl.C.Hkl
import           Hkl.DArray
import           Hkl.Parameter

------------
-- Engine --
------------

data Mode
  = Mode
    String --  name
    [Parameter] --  parameters of the @Mode@
  deriving (Show)

data Engine
  = Engine
    String --  name
    [Parameter] --  pseudo axes values of the @Engine@
    Mode --  current Mode
  deriving (Show)

engineName :: Engine -> String
engineName (Engine name _ _) = name

peekMode :: Ptr C'HklEngine -> IO Mode
peekMode e = do
  name <- c'hkl_engine_current_mode_get e >>= peekCString
  (DArray _ ns) <- peekDarrayString =<< c'hkl_engine_parameters_names_get e
  parameters <- mapM f ns
  return (Mode name parameters)
  where
    f n = c'hkl_engine_parameter_get e n nullPtr >>= peekParameter

peekEngine :: Ptr C'HklEngine -> IO Engine
peekEngine e = do
  name <- peekCString =<< c'hkl_engine_name_get e
  ps <- enginePseudoAxesGet e
  mode <- peekMode e
  return (Engine name ps mode)

-- engineNameGet :: Ptr HklEngine -> IO String
-- engineNameGet engine = c'hkl_engine_name_get engine >>= peekCString

-- enginePseudoAxisNamesGet :: Ptr HklEngine -> IO [String]
-- enginePseudoAxisNamesGet e = enginePseudoAxisNamesGet' e >>= mapM peekCString

enginePseudoAxisGet :: Ptr C'HklEngine -> CString -> IO Parameter
enginePseudoAxisGet e n = c'hkl_engine_pseudo_axis_get e n nullPtr >>= peekParameter

enginePseudoAxesGet :: Ptr C'HklEngine -> IO [Parameter]
enginePseudoAxesGet ptr = do
  (DArray _ ns) <- peekDarrayString =<< c'hkl_engine_pseudo_axis_names_get ptr
  mapM (enginePseudoAxisGet ptr) ns

engineSetValues :: Engine -> Vector Double -> Engine
engineSetValues (Engine name ps mode) vs = Engine name nps mode
  where
    nps = zipWith set ps (toList vs)
    set (Parameter n _ range) newValue = Parameter n newValue range

fromToPipe :: Int -> Vector Double -> Vector Double -> Producer (Vector Double) IO ()
fromToPipe n from to = forM_ [0..n-1] $ \i -> yield $ vs i
  where
    vs i = from + step * fromIntegral i
    step = (to - from) / (fromIntegral n - 1)

enginesTrajectoryPipe :: Engine -> Pipe (Vector Double) Engine IO ()
enginesTrajectoryPipe e = forever $ await >>= yield . engineSetValues e

----------------
-- EngineList --
----------------

-- withEngineList :: Factory -> (Ptr C'HklEngineList -> IO b) -> IO b
-- withEngineList f func = do
--   fptr <- newEngineList f
--   withForeignPtr fptr func

-- newEngineList :: Factory -> IO (ForeignPtr C'HklEngineList)
-- newEngineList f = newFactory f
--                   >>= c'hkl_factory_create_new_engine_list
--                   >>= newForeignPtr p'hkl_engine_list_free

-- engineListEnginesGet :: Ptr C'HklEngineList -> IO [Engine]
-- engineListEnginesGet e = do
--   (DArray n items) <- peek'darray_engine =<< c'hkl_engine_list_engines_get e
--   mapM peekEngine items

-- Engine

-- solve' :: Ptr C'HklEngine -> Engine -> IO (ForeignPtr C'HklGeometryList)
-- solve' engine (Engine _ ps _) = do
--   let positions = [v | (Parameter _ v _) <- ps]
--   let n = toEnum (length positions)
--   withArray positions $ \values ->
--       c'hkl_engine_pseudo_axis_values_set engine values n unit nullPtr
--       >>= newForeignPtr p'hkl_geometry_list_free

-- solve :: Geometry -> Detector a sh -> Sample -> Engine -> IO [Ptr C'HklGeometry]
-- solve g@(Geometry f _ _ _) d s e@(Engine name _ _) =
--   withSample s $ \sample ->
--       withDetector d $ \detector ->
--           withGeometry g $ \geometry ->
--               withEngineList f $ \engines ->
--                   withCString name $ \cname -> do
--                   c'hkl_engine_list_init engines geometry detector sample
--                   engine <- c'hkl_engine_list_engine_get_by_name engines cname nullPtr
--                   solve' engine e >>= peekHklGeometryList

-- solveTraj :: Geometry -> Detector a sh -> Sample -> [Engine] -> IO [Geometry]
-- solveTraj g@(Geometry f _ _ _) d s es = do
--   let name = engineName (head es)
--   withSample s $ \sample ->
--       withDetector d $ \detector ->
--           withGeometry g $ \geometry ->
--               withEngineList f $ \engines ->
--                 withCString name $ \cname -> do
--                   c'hkl_engine_list_init engines geometry detector sample
--                   engine <- c'hkl_engine_list_engine_get_by_name engines cname nullPtr
--                   mapM (solve' engine >=> getSolution0) es

-- Pipe

-- data Diffractometer = Diffractometer { difEngineList :: ForeignPtr C'HklEngineList
--                                      , difGeometry   :: ForeignPtr C'HklGeometry
--                                      , difDetector   :: ForeignPtr C'HklDetector
--                                      , difSample     :: ForeignPtr C'HklSample
--                                      }
--                     deriving (Show)

-- withDiffractometer :: Diffractometer -> (Ptr C'HklEngineList -> IO b) -> IO b
-- withDiffractometer d fun = do
--   let f_engines = difEngineList d
--   withForeignPtr f_engines fun

-- newDiffractometer :: Geometry -> Detector a sh -> Sample -> IO Diffractometer
-- newDiffractometer g@(Geometry f _ _ _) d s = do
--   f_engines <- newEngineList f
--   f_geometry <- newGeometry g
--   f_detector <- newDetector d
--   f_sample <- newSample s
--   withForeignPtr f_sample $ \sample ->
--     withForeignPtr f_detector $ \detector ->
--     withForeignPtr f_geometry $ \geometry ->
--     withForeignPtr f_engines $ \engines -> do
--       c'hkl_engine_list_init engines geometry detector sample
--       return $ Diffractometer { difEngineList = f_engines
--                               , difGeometry = f_geometry
--                               , difDetector = f_detector
--                               , difSample = f_sample
--                               }

-- computePipe :: Detector a sh -> Sample -> Pipe Geometry [Engine] IO ()
-- computePipe d s = forever $ do
--   g <- await
--   e <- lift $ compute g d s
--   yield e

-- solveTrajPipe :: Geometry -> Detector a sh -> Sample -> Pipe Engine Geometry IO ()
-- solveTrajPipe g d s = do
--   dif <- lift $ newDiffractometer g d s
--   solveTrajPipe' dif

-- solveTrajPipe' :: Diffractometer -> Pipe Engine Geometry IO ()
-- solveTrajPipe' dif = flip evalStateT dif $ forever $ do
--     -- Inside here we are using `StateT Diffractometer (Pipe Engine Geometry IO ()) r`
--     e <- lift await
--     dif_ <- get
--     let name = engineName e
--     solutions <- lift . lift $ withDiffractometer dif_ $ \engines ->
--      withCString name $ \cname -> do
--        engine <- c_hkl_engine_list_engine_get_by_name engines cname nullPtr
--        solve' engine e >>= getSolution0
--     put dif_
--     lift $ yield solutions

-- compute :: Geometry -> Detector a sh -> Sample -> IO [Engine]
-- compute g@(Geometry f _ _ _) d s =
--   withSample s $ \sample ->
--       withDetector d $ \detector ->
--           withGeometry g $ \geometry ->
--               withEngineList f $ \engines -> do
--                     c_hkl_engine_list_init engines geometry detector sample
--                     c_hkl_engine_list_get engines
--                     engineListEnginesGet engines
