{-# LANGUAGE CPP            #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

{-
    Copyright  : Copyright (C) 2014-2020, 2022, 2023 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Parameter
       ( Parameter(..)
       , Range(..)
       , copyParameter
       , peekParameter
       , pokeParameter
       ) where

import           Control.Monad         (void)
import           Data.Aeson            (FromJSON (..), ToJSON (..))
import           Foreign               (ForeignPtr, Ptr, newForeignPtr, nullPtr)
import           Foreign.C             (CDouble (CDouble))
import           Foreign.C.String      (peekCString)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Storable      (peek)
import           GHC.Generics          (Generic)

import           Hkl.C.Hkl

--  Range

data Range
  = Range
    Double --  minimum value
    Double --  maximum value
  deriving (Eq, FromJSON, Generic, Show, ToJSON)

--  Parameter

data Parameter
  = Parameter { parameterName  :: String
              , parameterValue :: Double
              , parameterRange :: Range
              }
  deriving (Eq, FromJSON, Generic, Show, ToJSON)

-- instance Storable Parameter where
--     alignment _ = alignment C'HklParameter
--     sizeOf _ = sizeOf C'HklParameter
--     peek ptr = alloca $ \pmin ->
--                alloca $ \pmax -> do
--                               cname <- c'hkl_parameter_name_get ptr
--                               name <- peekCString cname
--                               value <- c'hkl_parameter_value_get ptr unit
--                               c'hkl_parameter_min_max_get ptr pmin pmax unit
--                               min_ <- peek pmin
--                               max_ <- peek pmax
--                               return (Parameter name value (Range min_ max_))
--     poke ptr (Parameter _name value (Range min_ max_)) = do
--                               void $ c'hkl_parameter_value_set ptr (CDouble value) unit nullPtr
--                               void $ c'hkl_parameter_min_max_set ptr (CDouble min_) (CDouble max_) unit nullPtr

peekParameter :: Ptr C'HklParameter -> IO Parameter
peekParameter ptr =
  alloca $ \pmin ->
  alloca $ \pmax -> do
  cname <- c'hkl_parameter_name_get ptr
  name <- peekCString cname
  value <- c'hkl_parameter_value_get ptr c'HKL_UNIT_USER
  c'hkl_parameter_min_max_get ptr pmin pmax c'HKL_UNIT_USER
  (CDouble min_) <- peek pmin
  (CDouble max_) <- peek pmax
  return (Parameter name value (Range min_ max_))

pokeParameter :: Ptr C'HklParameter -> Parameter -> IO ()
pokeParameter ptr (Parameter _name value (Range min_ max_)) = do
  void $ c'hkl_parameter_value_set ptr (CDouble value) c'HKL_UNIT_USER nullPtr
  void $ c'hkl_parameter_min_max_set ptr (CDouble min_) (CDouble max_) c'HKL_UNIT_USER nullPtr

copyParameter :: Ptr C'HklParameter -> IO (ForeignPtr C'HklParameter)
copyParameter p = newForeignPtr p'hkl_parameter_free =<< c'hkl_parameter_new_copy p
