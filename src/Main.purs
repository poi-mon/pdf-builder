module Main where

import Control.Monad.Free
import Data.Map
import Prelude

import Control.Monad.State (StateT, get, lift, modify_, runStateT)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.Identity (Identity)
import Data.Maybe (fromMaybe, maybe)
import Data.String (joinWith)
import Data.Tuple (Tuple, fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  let e = runStateT encode { objs: [] }
  log $ show e
  -- let obj = mkDict [mkPair "Type" (Number 1.0), mkPair "Body" (mkArray [mkName "hello"])]
  -- log $ show obj

encode :: PdfM (Array Object)
encode = do
  pageTree <- mkObj $ Dictionary [mkPair "Kids" (Array []), mkPair "Count" (Number 1), mkPair "Type" (Name "Pages")]
  pageTreeRef <- ref pageTree
  documentCatalog <- mkObj $ Dictionary [mkPair "Pages" pageTreeRef, mkPair "Type" (Name "Catalog")]
  pure $ []

type PdfState = { objs :: Array Object }
type PdfM a = StateT PdfState Identity a

mkObj :: Primitive -> PdfM Ref
mkObj prim = do
  prev <- get
  let latest = maybe 1 (\(Object obj) -> obj.number + 1) $ maximum prev.objs
  let obj = Object { number: latest, gen: 0, inner: prim }
  modify_ $ \s -> s { objs = s.objs <> [obj] }
  pure latest

data Object = Object { number :: Int, gen :: Int, inner :: Primitive }
derive instance Eq Object
derive instance Ord Object

instance Show Object where
  show (Object obj) = "obj " <> show obj.number <> " " <> show obj.gen <> " " <> show obj.inner

type Ref = Int
ref :: Ref -> PdfM Primitive
ref number = do
  pure $ Ref number 0

data Primitive
  = Boolean Boolean
  | Null
  | Number Int
  | Name String
  | String String
  | Array (Array Primitive)
  | Dictionary (Array (Tuple String Primitive))
  | Ref Int Int
derive instance Eq Primitive
derive instance Ord Primitive
-- mkArray = Array

-- mkName :: String -> Object
-- mkName = Name

mkPair :: String -> Primitive -> Tuple String Primitive
mkPair key value = key /\ value

-- mkDict :: Array (Tuple String Object) -> Object
-- mkDict = Dictionary

instance Show Primitive where
  show (Boolean bool) = show bool
  show Null = "null"
  show (Number number) = "number"
  show (Name name) = "/" <> name
  show (String string) = string
  show (Array array) = "[" <> (joinWith " " $ map show array) <> "]"
  show (Dictionary dict) = "<<\n" <> (joinWith "\n" $ map (\(key /\ value) -> "/" <> key <> " " <> show value) dict) <> "\n>>"
  show (Ref number gen) = "ref " <> show number