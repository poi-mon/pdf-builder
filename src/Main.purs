module Main where

import Control.Monad.Free
import Data.Map
import Prelude

import Control.Monad.State (StateT, get, lift, modify_, put, runStateT)
import Data.Array (last, tail, (:))
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Maybe (fromMaybe, maybe)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.Tuple (Tuple, fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  let a = runStateT (runFreeM runPdf encode) { objs: [], prims: [], refs: [], lazy_ref: [], parent: 0 } 
  log $ show a
  log ""

data PdfF a
  = MkObj (Pdf Unit) (Object -> a)
  | MkArray (Pdf Unit) a
  | MkNull a
  | MkRef Object a

-- mkObj :: Pdf Unit -> Pdf Object
-- mkObj = liftF $ MkObj 

type Pdf = Free PdfF

instance Functor PdfF where
  map f (MkObj p k) = MkObj p (f <<< k)
  map f (MkArray a x) = MkArray a (f x)
  map f (MkNull a) = MkNull (f a)
  map f (MkRef a x) = MkRef a (f x)

mkObj p = liftF $ MkObj p identity

mkNull = liftF $ MkNull unit

mkArray p = liftF $ MkArray p unit

mkRef target = liftF $ MkRef target unit

data Primitive
  = Null
  | Ref Int
  | Array (Array Primitive)
derive instance Generic Primitive _
instance Show Primitive where
  show obj = genericShow obj

type Object = { number :: Int, inner :: Array Primitive }

type Reference = { src :: Int, dst :: Int }

type PdfState = { objs :: Array Object, prims :: Array Primitive, refs :: Array Reference, lazy_ref :: Array Int, parent :: Int }

runPdf :: forall a. PdfF (Pdf a) -> StateT PdfState Identity (Pdf a)
runPdf f = go f
  where
    go (MkObj p contf) = do
      -- store <- get
      -- let prevNumber = maybe 0 (_.number) $ last store.objs
      -- modify_ $ \s -> s { parent = prevNumber + 1 }
      runFreeM runPdf p
      -- put store
      prev <- get
      let prevNumber = maybe 0 (_.number) $ last prev.objs
      let obj = { number: prevNumber + 1, inner: prev.prims }
      let ref = prev.refs <> map (\dst -> { src: prevNumber + 1, dst }) prev.lazy_ref
      modify_ $ \s -> s { objs = s.objs <> [obj], prims = [], refs = ref }
      pure (contf obj)
    go (MkArray p cont) = do
      store <- get
      put { objs: store.objs, prims: [], refs: store.refs, lazy_ref: store.lazy_ref, parent: store.parent }
      runFreeM runPdf p
      modify_ $ \s -> s { prims = store.prims <> [Array s.prims] }
      pure cont
    go (MkNull cont) = do
      modify_ $ \prev -> 
        prev { prims = prev.prims <> [Null] }
      pure cont
    go (MkRef target cont) = do
      modify_ $ \prev -> 
        prev { prims = prev.prims <> [Ref target.number] }
      pure cont

encode ∷ Pdf Unit
encode = do
  obj <- mkObj $ do
    mkNull
    mkNull
    mkArray $ do
      mkNull
      mkArray $ do
        mkNull

  page <- mkObj $ do
    mkNull
    mkRef obj

  pure unit
  -- pure obj