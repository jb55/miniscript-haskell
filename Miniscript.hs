{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Text.Megaparsec
import Data.Functor (($>))
import Text.Megaparsec.Char
import Data.Text (Text)
import Data.Void (Void)
import Data.ByteString (ByteString)

import qualified Data.ByteString.Char8 as BS

newtype Hex = Hex { getHexBS :: ByteString }
    deriving (Eq, Ord)

instance Show Hex where
    show (Hex bs) = BS.unpack bs

data MiniscriptError = BadHexLen
                     deriving (Show, Eq, Ord)

describeErr :: MiniscriptError -> String
describeErr BadHexLen = "bad hexadecimal length"

type Parser a = Parsec Void Text a

data Expr where
    Pk    :: Hex  -> Expr
    Multi :: Int  -> Hex    -> Hex -> Expr
    Time  :: Expr -> Expr
    Hash  :: Expr -> Expr
    And   :: Expr -> Expr   -> Expr
    Or    :: Expr -> Expr   -> Expr
    Aor   :: Expr -> Expr   -> Expr
    Thres :: Int  -> [Expr] -> Expr
    deriving (Show, Eq, Ord)

hexString :: Parser Hex
hexString =
  fmap (Hex . BS.pack . concat) pairs
  where
    pairs = many ((:) <$> hexDigitChar <*> fmap (:[]) hexDigitChar)

pubkeyP :: Parser Hex
pubkeyP = do
  Hex h <- hexString
  if BS.length h /= 64
     then fail "pubkeys must be 32 bytes"
     else return (Hex h)

exprP :: Parser Expr
exprP = pkP <|> orP

orP :: Parser Expr
orP = do
  _  <- string "or"
  _  <- char '('
  e1 <- exprP
  _  <- char ','
  e2 <- exprP
  _  <- char ')'
  return (Or e1 e2)

testKeyP :: Parser Hex
testKeyP = char 'C' $> Hex "1212121212121212121212121212121212121212121212121212121212121212"

keyP :: Parser Hex
keyP = testKeyP <|> pubkeyP

pkP :: Parser Expr
pkP = do
  _ <- string "pk("
  h <- keyP
  _ <- char ')'
  return (Pk h)

main :: IO ()
main = print $ parse pkP "" "pk(C)"
