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
import Data.Functor.Identity (Identity)
import Data.Void (Void)
import Data.ByteString (ByteString)

import qualified Data.Text as T

import qualified Data.ByteString.Char8 as BS

newtype Pubkey = Pubkey { getPubkeyHex :: ByteString }
    deriving (Eq, Ord)

newtype SHA256 = SHA256 { getSHA256 :: ByteString }
    deriving (Eq, Ord)

instance Show Pubkey where
    show (Pubkey bs) = BS.unpack bs

instance Show SHA256 where
    show (SHA256 bs) = BS.unpack bs

infixr 5 ?:
(?:) :: Maybe a -> [a] -> [a]
Just x  ?: xs = x : xs
Nothing ?: xs = xs

type Parser a = Parsec Void Text a

data Expr where
    Pk    :: Pubkey -> Expr
    Multi :: Int    -> [Pubkey] -> Expr
    Time  :: Int    -> Expr
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

hexP :: String -> (ByteString -> a) -> Parser a
hexP name make = do
  h <- hexString
  if BS.length h /= 64
     then fail (name ++ " must be 32 bytes")
     else return (make h)

sha256P :: Parser SHA256
sha256P = hexP "sha256" SHA256

pubkeyP :: Parser Pubkey
pubkeyP = hexP "pubkey" Pubkey

testSHA256P :: Parser SHA256
testSHA256P = char 'H' $> SHA256 "8888888888888888888888888888888888888888888888888888888888888888"

hashValP :: Parser SHA256
hashValP = testSHA256P <|> sha256P

exprP :: Parser Expr
exprP = pkP <|> andP <|> orP <|> hashP <|> timeP <|> multiP

desc :: String -> ParsecT Void Text Identity a -> ParsecT Void Text Identity a
desc = flip (<?>)

binP :: Text -> (Expr -> Expr -> Expr) -> Parser Expr
binP name make = desc (T.unpack name) $ do
  _  <- string name
  e1 <- exprP
  _  <- char ','
  e2 <- exprP
  _  <- char ')'
  return (make e1 e2)

andP, orP, aorP :: Parser Expr
andP = binP "and" And
orP  = binP "or" Or
aorP = binP "aor" Aor

testKeyP :: Parser Pubkey
testKeyP = desc "C" $
    char 'C' $> Pubkey "1212121212121212121212121212121212121212121212121212121212121212"

keyP :: Parser Pubkey
keyP = testKeyP <|> pubkeyP

multiP :: Parser Expr
multiP = desc "multi" $ do
  _  <- string "multi("
  n  <- decimal
  _  <- char ','
  es <- keyP `sepBy` char ','
  _  <- char ')'
  let nkeys = length es
  if n == 0 || n > nkeys
     then fail ("asking for " ++ show n ++ " keys to sign, but provided " ++ show nkeys)
     else return (Multi n es)

unaryP :: Text -> Parser a -> (a -> Expr) -> Parser Expr
unaryP name body make = desc (T.unpack name) $ do
  _ <- string name
  _ <- char '('
  n <- body
  _ <- char ')'
  return (make n)

timeP, pkP, hashP :: Parser Expr
timeP = unaryP "time" decimal  Time
pkP   = unaryP "pk"   keyP     Pk
hashP = unaryP "hash" hashValP Hash

main :: IO ()
main = print (parse pkP "" "pk(C)")
