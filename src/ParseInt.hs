-- realworldhaskell ch 19
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoDerivingVia #-}
module ParseInt
    ( satisfy
    , runParser
    , optional
    ) where
import Control.Monad.Error
import Control.Monad.State
import qualified Data.ByteString.Char8 as B

data ParseError = NumericOverflow
                | EndOfInput
                | Chatty String
                  deriving (Eq, Ord, Show)
instance Error ParseError where
  noMsg = Chatty "oh noes!"
  strMsg = Chatty
newtype Parser a = P {
  runP :: ErrorT ParseError (State B.ByteString) a
  } deriving (Monad, MonadError ParseError)

instance Functor Parser where
  fmap = fmap
instance Applicative Parser where
  pure = return
  (<*>) = ap
liftP :: State B.ByteString a -> Parser a
liftP m = P (lift m)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  s <- liftP get
  case B.uncons s of
    Nothing -> throwError EndOfInput
    Just (c, s')
      | p c -> liftP (put s') >> return c
      | otherwise -> throwError (Chatty "satisfy failed")
optional :: Parser a -> Parser (Maybe a)
optional p = (Just `liftM` p) `catchError` \_ -> return Nothing
  
runParser :: Parser a -> B.ByteString -> Either ParseError (a, B.ByteString )
runParser p bs = case runState (runErrorT (runP p)) bs of
  (Left err, _) -> Left err
  (Right r, bs) -> Right (r, bs)
