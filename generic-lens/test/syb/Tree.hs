{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-
Example adapted from SYB
========================
-}

module Main (main) where

import Control.Lens
import Control.Monad (void)
import Data.Generics.Product
import GHC.Generics
import Test.HUnit hiding (State)
import Control.Monad.State

main :: IO ()
main = void $ runTestTT tests

-- A parameterised datatype for binary trees with data at the leafs
data Tree a w = Leaf a
              | Fork (Tree a w) (Tree a w)
              | WithWeight (Tree a w) w
       deriving (Show, Generic, Eq)

type Parameter = Int

data A = A {
  a_p0 :: Parameter,
  a_p1 :: B
} deriving (Show, Generic, Eq)

data B = B {
  b_p0 :: Parameter
} deriving (Show, Generic, Eq)

pop :: forall s. State [s] s
pop = do
  params <- get
  case params of
    [] -> error "Not enough parameters supplied to replaceParameters"
    (p : t) -> do put t; return p

update :: forall a s. HasTypes s a => s -> State [a] s
update s = (types @a) (\_ -> pop) s
  
replaceAll :: forall a s. HasTypes s a => s -> [a] -> s
replaceAll f params =
  let (f', remaining) = runState (update f) params
   in if null remaining
        then f'
        else error "Some parameters in a call to replaceParameters haven't been consumed!"

a :: A
a = A 3 (B 4)

-- A typical tree
mytree :: Tree Int Int
mytree = Fork (WithWeight (Leaf 42) 1)
              (WithWeight (Fork (Leaf 88) (Leaf 37)) 2)

mytreeShown :: Tree String Int
mytreeShown = Fork (WithWeight (Leaf "42") 1)
                (WithWeight (Fork (Leaf "88") (Leaf "37")) 2)



-- A polymorphic-recursive structure
data Poly a b
  = PNil
  | PCons a (Poly b a)
  deriving (Show, Generic)

poly :: Poly Int String
poly = PCons 10 (PCons "hello" (PCons 20 (PCons "world" PNil)))

-- Print everything like an Int in mytree
-- In fact, we show two attempts:
--   1. print really just everything like an Int
--   2. print everything wrapped with Leaf
-- So (1.) confuses leafs and weights whereas (2.) does not.
tests :: Test
tests = TestList
  [ toListOf (types @Int) mytree ~=? [42,1,88,37,2]
  , toListOf (param @1) mytree       ~=? [42,88,37]

  -- Things not (easily) doable in SYB:
  -- change type of Tree by mapping a function over the second (from the right) param
  , (mytree & param @1 %~ show)                           ~=? mytreeShown
  -- collect values in poly corresponding to the first param
  , toListOf (param @0) poly                              ~=? ["hello", "world"]
  -- collect all Ints inside poly
  , toListOf (types @Int) poly                        ~=? [10, 20]
  -- map length over the Strings, then collect all Ints
  , toListOf (types @Int) (poly & param @0 %~ length) ~=? [10, 5, 20, 5]
  -- map length over the Strings, then collect all the resulting Ints
  , toListOf (param @0) (poly & param @0 %~ length)       ~=? [5,5]
  , toListOf (types @Int) a ~=? [3,4]
  , toListOf (types @Int) (replaceAll @Int a [5,6]) ~=? [5,6]
  ]

-- original code from SYB:
--tests = show ( listify (\(_::Int) -> True)         mytree
--             , everything (++) ([] `mkQ` fromLeaf) mytree
--             ) ~=? output
--  where
--    fromLeaf :: Tree Int Int -> [Int]
--    fromLeaf (Leaf x) = [x]
--    fromLeaf _        = []
