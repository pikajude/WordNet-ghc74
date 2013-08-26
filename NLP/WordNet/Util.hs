module NLP.WordNet.Util where

import NLP.WordNet.PrimTypes

#if MIN_VERSION_base(4,6,0)
import Prelude
#else
import Prelude hiding (catch)
#endif
import Control.Exception
import Control.Monad
import Data.Char (toLower)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.String
import GHC.IO.Handle
import System.IO

data IOModeEx = BinaryMode IOMode | AsciiMode IOMode deriving (Eq, Ord, Show, Read)

openFileEx :: FilePath -> IOModeEx -> IO Handle
openFileEx fp (BinaryMode md) = openBinaryFile fp md
openFileEx fp (AsciiMode  md) = openFile fp md


fst3 :: (a, b, c) -> a
fst3 (a,_,_) = a
snd3 :: (a, b, c) -> b
snd3 (_,b,_) = b
thr3 :: (a, b, c) -> c
thr3 (_,_,c) = c

maybeRead :: (Read a, Monad m) => String -> m a
maybeRead s = 
  case reads s of
    (a,_):_ -> return a
    _       -> fail "error parsing string"

matchN :: Monad m => Int -> [a] -> m [a]
matchN n l | length l >= n = return l
           | otherwise     = fail "expecting more tokens"

lexId :: Synset -> Int -> Int
lexId x n = (\ (_,i,_) -> i) (ssWords x !! n)

padTo :: Int -> String -> String
padTo n s = reverse $ take n (reverse s ++ repeat '0')
      
sensesOf :: Int {- num senses -} -> SenseType -> [Int]
sensesOf n AllSenses = [1..n]
sensesOf n (SenseNumber i)
    | i <= 0 = []
    | i >  n = []
    | otherwise = [i]

-- utility functions

charForPOS :: IsString a => POS -> a
charForPOS (Noun) = "n"
charForPOS (Verb) = "v"
charForPOS (Adj)  = "a"
charForPOS (Adv)  = "r"

tryMaybe :: IO a -> IO (Maybe a)
tryMaybe a = liftM Just a `catch` (\(_ :: SomeException) -> return Nothing)

tryMaybeWarn :: Exception e => (e -> IO ()) -> IO a -> IO (Maybe a)
tryMaybeWarn warn a = liftM Just a `catch` (\e -> warn e >> return Nothing)

partName :: POS -> String
partName = map toLower . show

cannonWNString :: String -> [String]
cannonWNString s'
    | notElem '_' s &&
      notElem '-' s &&
      notElem '.' s = [s]
    | otherwise = 
        nub [s, 
             replaceChar '_' '-' s,
             replaceChar '-' '_' s,
             filter (not . (`elem` "_-")) s,
             filter (/='.') s
            ]
  where s = map toLower s'

replaceChar :: Eq a => a -> a -> [a] -> [a]
replaceChar _ _ [] = []
replaceChar from to (c:cs)
    | c == from = to : replaceChar from to cs
    | otherwise = c  : replaceChar from to cs

getPointerType :: (Eq a, IsString a) => a -> Form
getPointerType s = fromMaybe Unknown $ lookup s l
  where
    l = 
       [("!",   Antonym),
        ("@",   Hypernym),
        ("~",   Hyponym),
        ("*",   Entailment),
        ("&",   Similar),
        ("#m",  IsMember),
        ("#s",  IsStuff),
        ("#p",  IsPart),
        ("%m",  HasMember),
        ("%s",  HasStuff),
        ("%p",  HasPart),
        ("%",   Meronym),
        ("#",   Holonym),
        (">",   CauseTo),
        ("<",   PPL),
        ("^",   SeeAlso),
--        ("\\",  Pertainym),
        ("=",   Attribute),
        ("$",   VerbGroup),
        ("+",   Nominalization),
        (";",   Classification),
        ("-",   Class),
        -- additional searches, but not pointers.
        ("+",    Frames)]
