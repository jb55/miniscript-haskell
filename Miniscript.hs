{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Text.Megaparsec
import Data.Functor (($>))
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Text (Text)
import Data.Void (Void)
import Data.ByteString (ByteString)

import qualified Data.ByteString.Char8 as BS

newtype Pubkey = Pubkey { getPubkeyHex :: ByteString }
    deriving (Eq, Ord)

newtype SHA256 = SHA256 { getSHA256 :: ByteString }
    deriving (Eq, Ord)

instance Show Pubkey where
    show (Pubkey bs) = BS.unpack bs

instance Show SHA256 where
    show (SHA256 bs) = BS.unpack bs

data MiniscriptError = BadHexLen
                     deriving (Show, Eq, Ord)

describeErr :: MiniscriptError -> String
describeErr BadHexLen = "bad hexadecimal length"

type Parser a = Parsec Void Text a

data Expr where
    Pk    :: Pubkey -> Expr
    Multi :: Int    -> [Pubkey] -> Expr
    Time  :: Expr   -> Expr
    Hash  :: SHA256 -> Expr
    And   :: Expr   -> Expr   -> Expr
    Or    :: Expr   -> Expr   -> Expr
    Aor   :: Expr   -> Expr   -> Expr
    Thres :: Int    -> [Expr] -> Expr
    deriving (Show, Eq, Ord)

hexString :: Parser ByteString
hexString =
  fmap (BS.pack . concat) pairs
  where
    pairs = many ((:) <$> hexDigitChar <*> fmap (:[]) hexDigitChar)

pubkeyP :: Parser Pubkey
pubkeyP = do
  h <- hexString
  if BS.length h /= 64
     then fail "pubkeys must be 32 bytes"
     else return (Pubkey h)

sha256P :: Parser SHA256
sha256P = do
  h <- hexString
  if BS.length h /= 64
     then fail "sha256 hashes must be 32 bytes"
     else return (SHA256 h)

testSHA256P :: Parser SHA256
testSHA256P = char 'H' $> SHA256 "8888888888888888888888888888888888888888888888888888888888888888"

hashValP :: Parser SHA256
hashValP = testSHA256P <|> sha256P

hashP :: Parser Expr
hashP = do
  _   <- string "hash("
  hex <- hashValP
  _   <- char ')'
  return (Hash hex)

exprP :: Parser Expr
exprP = pkP <|> orP <|> hashP <|> multiP

orP :: Parser Expr
orP = do
  _  <- string "or("
  e1 <- exprP
  _  <- char ','
  e2 <- exprP
  _  <- char ')'
  return (Or e1 e2)

testKeyP :: Parser Pubkey
testKeyP = char 'C' $> Pubkey "1212121212121212121212121212121212121212121212121212121212121212"

keyP :: Parser Pubkey
keyP = testKeyP <|> pubkeyP

multiP :: Parser Expr
multiP = do
  _  <- string "multi("
  n  <- decimal
  _  <- char ','
  es <- keyP `sepBy` char ','
  _  <- char ')'
  let nkeys = length es
  if n == 0 || n > nkeys
     then fail ("asking for " ++ show n ++ " keys to sign, but provided " ++ show nkeys)
     else return (Multi n es)

pkP :: Parser Expr
pkP = do
  _ <- string "pk("
  h <- keyP
  _ <- char ')'
  return (Pk h)

main :: IO ()
main = print $ parse pkP "" "pk(C)"
