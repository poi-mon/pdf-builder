module Main where

import Control.Monad.Free
import Data.Map
import Prelude

import Control.Monad.State (StateT, get, lift, modify_, put, runStateT)
import Data.Array (head, last, tail, (:))
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Profunctor (arr)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.Tuple (Tuple, fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = do
  let a = runPdf' encode
  log $ show a

encode = do
  page <- obj $ array $ do
    number 1.0
    name "l"
  tree <- obj $ dict $ do
    "Page" >> (ref page)
  pure unit

type Ref = Int

data Incomplete
  = Boolean_ Boolean
  | Null_
  | Number_ Number
  | Integer_ Int
  | Name_ String
  | Array_ (Array Incomplete)
  | Map_ (Array Incomplete)
  | Pair_ String Incomplete
  | Ref_ Ref
  | ParentRef_

derive instance Generic Incomplete _
instance Show Incomplete where
  show i = genericShow i

data Complete
  = Boolean Boolean
  | Null
  | Number Number
  | Integer Int
  | Name String
  | Array (Array Complete)
  | Map (Array (Tuple String Complete))
  | Ref Ref Ref

instance Show Complete where
  show (Boolean bool) = show bool
  show Null = "null"
  show (Number number) = "number"
  show (Integer integer) = "integer"
  show (Name name) = "/" <> name
  show (Array array) = "[" <> (joinWith " " $ map show array) <> "]"
  show (Map dict) = "<<\n" <> (joinWith "\n" $ map (\(key /\ value) -> "/" <> key <> " " <> show value) dict) <> "\n>>"
  show (Ref number gen) = "ref " <> show number

data Object = Object { number :: Int, generation :: Int, inner :: Array Incomplete }
derive instance Generic Object _
instance Show Object where
  show o = genericShow o

type Pdf = Free PdfF

data PdfF a
  = MkObj (Pdf Unit) (Object -> a)
  | MkBoolean Boolean a
  | MkNumber Number a
  | MkInteger Int a
  | MkName String a
  | MkArray (Pdf Unit) a
  | MkMap (Pair Unit) a
  | MkPair String (Pdf Unit) a
  | MkRef Object a
  | MkParentRef a

type Pair = Free PairF

data PairF a
  = Pair String (Pdf Unit) a

instance Functor PairF where
  map f (Pair k m a) = Pair k m (f a)

instance Functor PdfF where
  map f (MkObj m k) = MkObj m (f <<< k)
  map f (MkBoolean x a) = MkBoolean x (f a)
  map f (MkNumber x a) = MkNumber x (f a)
  map f (MkInteger x a) = MkInteger x (f a)
  map f (MkName x a) = MkName x (f a)
  map f (MkArray m a) = MkArray m (f a)
  map f (MkMap m a) = MkMap m (f a)
  map f (MkPair k v a) = MkPair k v (f a)
  map f (MkRef x a) = MkRef x (f a)
  map f (MkParentRef a) = MkParentRef (f a)

obj m = liftF $ MkObj m identity

bool b = liftF $ MkBoolean b unit

number n = liftF $ MkNumber n unit

integer i = liftF $ MkInteger i unit

name n = liftF $ MkName n unit

array m = liftF $ MkArray m unit

dict m = liftF $ MkMap m unit

pair k v = liftF $ Pair k v unit
infixr 3 pair as >>

ref t = liftF $ MkRef t unit

parentRef = liftF $ MkParentRef unit

type Reference = { src :: Ref, dst :: Ref }

type PdfState = { objs :: Array Object, stack :: Array (Array Incomplete), cur :: Array Incomplete, objNumber :: Ref, xref :: Array Reference }

runPdf' m = runStateT (runFreeM runPdf m) { objs: [], stack: [[]], cur: [], objNumber: 1, xref: [] }

runPdf :: forall a. PdfF (Pdf a) -> StateT PdfState Identity (Pdf a)
runPdf m = go m
  where
    go (MkObj m cont) = do
      prev <- get
      let prevNumber = fromMaybe 0 $ maximum (map (\(Object o) -> o.number ) prev.objs)
      let selfNumber = prevNumber + 1
      setObjNumber selfNumber
      saveState
      runFreeM runPdf m
      m' <- restoreState
      let newObj = Object { number: selfNumber, generation: 0, inner: m' }
      pushObj newObj
      pure $ cont newObj
    go (MkBoolean bool cont) = do
      push $ Boolean_ bool
      pure cont
    go (MkNumber number cont) = do
      push $ Number_ number
      pure cont
    go (MkInteger integer cont) = do
      push $ Integer_ integer
      pure cont
    go (MkName name cont) = do
      push $ Name_ name
      pure cont
    go (MkArray m cont) = do
      saveState
      runFreeM runPdf m
      arr <- restoreState
      push $ Array_ arr
      pure cont
    go (MkMap m cont) = do
      saveState
      runFreeM runPair m
      pairs <- restoreState
      push $ Map_ pairs
      pure cont
    go (MkPair k v cont) = do
      v' <- call v
      push $ Pair_ k v'
      pure cont
    go (MkRef (Object t) cont) = do
      push $ Ref_ t.number
      appendXref t.number =<< getSelfNumber
      pure cont
    go (MkParentRef cont) = do
      push $ ParentRef_
      pure cont

    push incomp = do
      modify_ $ \s -> s { cur = incomp : s.cur }
    
    pushObj obj = do
      modify_ $ \s -> s { objs = s.objs <> [obj] }
    
    call m = do
      saveState
      runFreeM runPdf m
      a <- restoreState
      -- TODO
      pure $ unsafePartial $ fromJust $ head a

    getSelfNumber = do
      state <- get
      pure $ state.objNumber

    appendXref dst src = do
      modify_ $ \s -> s { xref = s.xref <> [ { src, dst } ]}

    saveState = do
      prev <- get
      modify_ $ \s -> s { stack = prev.cur : s.stack, cur = [] }
    
    restoreState = do
      state <- get
      modify_ $ \s -> s { stack = fromMaybe [] $ tail s.stack, cur = fromMaybe [] $ head s.stack }
      pure $ state.cur
    
    setObjNumber number = do
      modify_ $ \s -> s { objNumber = number }

    runPair (Pair k m cont) = do
      v <- call m
      push $ Pair_ k v
      pure cont

resolveParentRef :: Array Incomplete -> Array Reference -> Array Complete
resolveParentRef incomps xref = []