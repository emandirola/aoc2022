{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, InstanceSigs, RankNTypes #-}
module Utils where

class Show a => ToList a where
  toList :: a -> [String]

instance Show a => ToList [a] where
  toList = map show

instance Show a => ToList (a, a) where
  toList (x, y) = [show x, show y]
