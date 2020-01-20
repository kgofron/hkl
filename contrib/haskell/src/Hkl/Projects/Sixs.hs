{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

{-
    Copyright  : Copyright (C) 2014-2020 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
module Hkl.Projects.Sixs
    ( main_sixs )
        where
import           Control.Concurrent.Async          (mapConcurrently)
import           Control.Monad                     (forM_, forever)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Data.Array.Repa                   (Array, Shape, extent,
                                                    listOfShape, size)
import           Data.Array.Repa.Index             (DIM1, DIM2, DIM3, Z)
import           Data.Array.Repa.Repr.ForeignPtr   (F, toForeignPtr)
import           Data.Vector.Storable              (concat, head)
import           Data.Word                         (Word16)
import           Foreign.C.Types                   (CInt (..))
import           Foreign.ForeignPtr                (ForeignPtr, withForeignPtr)
import           Foreign.Marshal.Array             (withArrayLen)
import           Foreign.Ptr                       (Ptr)
import           Foreign.Storable                  (peek)
import           GHC.Conc                          (getNumCapabilities)
import           Hkl
import           Numeric.LinearAlgebra             (Matrix)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (Angle, Length, degree,
                                                    meter, (*~), (/~))
import           Pipes                             (Pipe, await, each, yield,
                                                    (>->))
import           Pipes.Prelude                     (mapM, toListM)
import           Pipes.Safe                        (SafeT, runSafeT)
import           Text.Printf                       (printf)

import           Prelude                           hiding (mapM)

data Chunk n a = Chunk !a !n !n
deriving instance (Show n, Show a) => Show (Chunk n a)

cweight :: Num n => Chunk n a -> n
cweight (Chunk _ l h) = h - l

csplit :: Num n => Chunk n a -> n -> (Chunk n a, Chunk n a)
csplit (Chunk a l h) n = ( (Chunk a l (l + n)), (Chunk a (l+n) h) )

chunk :: (Num n, Ord n) => n -> [Chunk n a] -> [[Chunk n a]]
chunk target = go target target
  where
    go _ _ []          = []
    go tgt gap [x]     = golast tgt gap x
    go tgt gap ~(x:xs) =
      let gap' = gap - cweight x
      in if | gap' > 0                -> cons1 x $ go tgt gap' xs
            | gap' == 0                -> [x] : go tgt tgt xs
            | (x1, x2) <- csplit x gap -> [x1] : go tgt tgt (x2 : xs)

    cons1 !x ~(c:cs) = (x : c) : cs

    golast tgt gap x =
      if | cweight x <= gap         -> [x] : []
         | (x1, x2) <- csplit x gap -> [x1] : golast tgt tgt x2

{-# SPECIALIZE chunk :: Int -> [Chunk Int FilePath] -> [[Chunk Int FilePath]]  #-}

data DataFrame
    = DataFrame
      Int -- n
      Geometry -- geometry
      (Matrix Double) -- ub
      (ForeignPtr Word16) -- image

instance Show DataFrame where
  show (DataFrame i g m _) = unwords [show i, show g, show m]

class FramesP a where
  lenP :: a -> Pipe FilePath Int (SafeT IO) ()
  framesP :: a -> Detector b DIM2 -> Pipe (Chunk Int FilePath) DataFrame (SafeT IO) ()

data DataFrameHklH5Path
  = DataFrameHklH5Path
    (Hdf5Path DIM3 Word16) -- Image
    (Hdf5Path DIM1 Double) -- Mu
    (Hdf5Path DIM1 Double) -- Omega
    (Hdf5Path DIM1 Double) -- Delta
    (Hdf5Path DIM1 Double) -- Gamma
    (Hdf5Path DIM2 Double) -- UB
    (Hdf5Path Z Double) -- Wavelength
    (Hdf5Path DIM1 Char) -- DiffractometerType

instance FramesP DataFrameHklH5Path where
  lenP (DataFrameHklH5Path _ m _ _ _ _ _ _) = forever $ do
    fp <- await
    withFileP (openH5 fp) $ \f ->
      withHdf5PathP f m $ \m' -> do
      (Just n) <- liftIO $ lenH5Dataspace m'
      yield n

  framesP (DataFrameHklH5Path i m o d g u w t) det = forever $ do
    (Chunk fp from to) <- await
    withFileP (openH5 fp) $ \f ->
      withHdf5PathP f i $ \i' ->
      withHdf5PathP f m $ \m' ->
      withHdf5PathP f o $ \o' ->
      withHdf5PathP f d $ \d' ->
      withHdf5PathP f g $ \g' ->
      withHdf5PathP f u $ \u' ->
      withHdf5PathP f w $ \w' ->
      withHdf5PathP f t $ \_t' -> do
      forM_ [from..to-1] (\j -> yield =<< liftIO
                       (do
                           mu <- get_position m' j
                           omega <- get_position o' j
                           delta <- get_position d' j
                           gamma <- get_position g' j
                           wavelength <- get_position w' 0
                           image <- get_image' det i' j
                           ub <- get_ub u'
                           let positions = Data.Vector.Storable.concat [mu, omega, delta, gamma]
                               source = Source (Data.Vector.Storable.head wavelength *~ angstrom)
                           pure $ DataFrame j (Geometry Uhv source positions Nothing) ub image))

-- | DataFrameSpace

data DataFrameSpace sh = DataFrameSpace (ForeignPtr Word16) (Space sh)
  deriving Show

type Resolutions = [Double]

{-# INLINE space #-}
space :: Detector a DIM2 -> Array F DIM3 Double -> Resolutions -> DataFrame -> IO (DataFrameSpace DIM3)
space detector pixels rs (DataFrame _ g@(Geometry _ (Source w) _ _) _ub img) = do
  let k = 2 * pi / (w /~ angstrom)
  let nPixels = size . shape $ detector
  let pixelsDims = map toEnum $ listOfShape . extent $ pixels :: [CInt]
  withGeometry g $ \geometry ->
    withForeignPtr (toForeignPtr pixels) $ \pix ->
    withArrayLen rs $ \nr r ->
    withArrayLen pixelsDims $ \ndim dims ->
    withForeignPtr img $ \i -> do
      p <- {-# SCC "hkl_binoculars_space_q" #-} hkl_binoculars_space_q geometry k i (toEnum nPixels) pix (toEnum ndim) dims r (toEnum nr)
      s <- peek p
      return (DataFrameSpace img s)

spaceP :: (MonadIO m) => Detector a DIM2 -> Array F DIM3 Double -> Resolutions -> Pipe DataFrame (DataFrameSpace DIM3) m ()
spaceP detector pixels rs = mapM (\r -> liftIO $ space detector pixels rs r)

-- | Create the Cube

withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO r) -> IO r
withForeignPtrs []       f = f []
withForeignPtrs (fp:fps) f =
  withForeignPtr fp $ \p ->
  withForeignPtrs fps $ \ps -> f (p:ps)

{-# INLINE mkCube' #-}
mkCube' :: Shape sh => Detector a DIM2 -> [DataFrameSpace sh] -> IO (Cube' sh)
mkCube' detector dfs = do
  let spaces = [spaceHklPointer s | (DataFrameSpace _ s) <- dfs]
  let images = [img | (DataFrameSpace img _) <- dfs]
  let nPixels = size . shape $ detector
  withForeignPtrs spaces $ \pspaces ->
    withForeignPtrs images $ \pimages ->
    withArrayLen pspaces $ \nSpaces' spaces' ->
    withArrayLen pimages $ \_ images' -> do
    peek =<< {-# SCC "hkl_binoculars_cube_new'" #-} hkl_binoculars_cube_new' (toEnum nSpaces') spaces' (toEnum nPixels) images'

type Template = String

data InputFn = InputFn FilePath
             | InputRange Template Int Int


toList :: InputFn -> [FilePath]
toList (InputFn f)           = [f]
toList (InputRange tmpl f t) = [printf tmpl i | i <- [f..t]]

data Input = Input { filename     :: InputFn
                   , h5path       :: DataFrameHklH5Path
                   , output       :: FilePath
                   , resolutions  :: [Double]
                   , centralPixel :: (Int, Int)  -- x, y
                   , sdd          :: (Length Float)  -- sample to detector distance
                   , detrot       :: Angle Float
                   }

_manip1 :: Input
_manip1 = Input { filename = InputFn "/nfs/ruche-sixs/sixs-soleil/com-sixs/2015/Shutdown4-5/XpadAu111/align_FLY2_omega_00045.nxs"
                , h5path = DataFrameHklH5Path
                           (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                           (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_MU")
                           (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_OMEGA")
                           (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_DELTA")
                           (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_GAMMA")
                           (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1/" $ datasetp "UB")
                           (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength")
                           (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1" $ datasetp "type")
                , output = "test1.hdf5"
                , resolutions = [0.002, 0.002, 0.002]
                , centralPixel = (0, 0)
                , sdd = 1 *~ meter
                , detrot = 90 *~ degree
                }

manip2 :: Input
manip2 = Input { filename = InputRange "/nfs/ruche-sixs/sixs-soleil/com-sixs/2019/Run3/FeSCO_Cu111/sample2_ascan_omega_%05d.nxs" 77 93
               , h5path = DataFrameHklH5Path
                          (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                          (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                          (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                          (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                          (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                          (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx2-ex-diff-uhv" $ datasetp "UB")
                          (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda")
                          (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx2-ex-diff-uhv" $ datasetp "type")
               , output = "test2.hdf5"
               , resolutions = [0.003, 0.01, 0.003]
               , centralPixel = (352, 112)
               , sdd = 1.162 *~ meter
               , detrot = 90 *~ degree
               }

-- manip3 :: Input
-- manip3 = Input { filename = InputRange "/nfs/ruche-sixs/sixs-soleil/com-sixs/2018/Run3/Corentin/Al13Co4/Al13Co4_ascan_omega_%05d.nxs" 4 137

--                }

mkJobs' :: Int -> [FilePath] -> [Int] -> [[Chunk Int FilePath]]
mkJobs' n fns ts = chunk n [Chunk f 0 t | (f, t) <- zip fns ts]

mkJobs :: Input -> IO [[Chunk Int FilePath]]
mkJobs i = do
  let fns = toList $ filename i
  ns <- runSafeT $ toListM $
       each fns
       >-> lenP (h5path i)
  c <- getNumCapabilities
  let ntot = foldl (+) 0 ns
  return $ mkJobs' (quot ntot c) fns ns

main_sixs' :: IO ()
main_sixs' = do
  let input = manip2
  let detector = ImXpadS140

  pixels <- getPixelsCoordinates detector (centralPixel input) (sdd input) (detrot input)

  jobs <- mkJobs input
  r' <- mapConcurrently (\job -> runSafeT $ toListM $
                               each job
                               >-> framesP (h5path input) detector
                               >-> spaceP detector pixels (resolutions input)
                       ) jobs
  c' <- mconcat <$> mapConcurrently (mkCube' detector) r'
  c <- toCube c'
  saveHdf5 (output input) c
  print c

  return ()

main_sixs :: IO ()
main_sixs = main_sixs'
