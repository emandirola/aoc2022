{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Utils where

import qualified Data.ByteString.Char8 as BS

class ConvertBS a where
  toByteString :: a -> BS.ByteString
  fromByteString :: BS.ByteString -> a

instance ConvertBS String where
  toByteString = BS.pack
  fromByteString = BS.unpack

instance ConvertBS BS.ByteString where
  toByteString = id
  fromByteString :: BS.ByteString -> BS.ByteString
  fromByteString = id