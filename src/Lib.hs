{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Lib
    ( someFunc
    , divBy
    ) where
import Control.Monad.Error
data DivByError a = DivBy0
                            | ForbiddenDenominator a
                            | OtherDivByError String
                              deriving (Eq, Read, Show)
instance Error (DivByError a) where
  strMsg = OtherDivByError
divBy :: Integral a => a -> [a] -> Either (DivByError a) [a]
divBy = divByGeneric
divByGeneric :: (Integral a, MonadError (DivByError a) m) =>
                a -> [a] -> m [a]
divByGeneric _ [] = return []
divByGeneric _ (0:_) = throwError DivBy0
divByGeneric _ (10:_) = throwError (ForbiddenDenominator 10)
divByGeneric _ (20:_) = throwError (ForbiddenDenominator 20)
divByGeneric numerator (denom:xs) =
  do next <- divByGeneric numerator xs
     return ((numerator `div` denom) : next)
someFunc :: IO ()
someFunc = print "someFunc"
