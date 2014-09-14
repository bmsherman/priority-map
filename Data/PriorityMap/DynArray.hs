{- |
A dynamically resizeable array in the ST monad. Indices begin at 0.
-}
module Data.PriorityMap.DynArray where

import Control.Monad.ST (ST)
import Data.Array.ST
import Data.STRef

-- | A dynamically resizeable array in the ST monad.
data DynArray s e = DynArray
  { -- | A mutable reference to the current array that is in use.
  payload :: STRef s (STArray s Int e),
    -- | The growth strategy of the array. When the array is grown, this
    -- function takes the old array size and generates the new array size.
    -- This function should be monotonically increasing. 
  growth :: Int -> Int }

-- | Create a new, empty array, with a current capacity of 16, which doubles
-- in size whenever it exceeds capacity.
empty :: ST s (DynArray s e)
empty = emptyWith 16 (2*)

-- | Create a new, empty array with a particular size and growth strategy.
emptyWith :: Int -> (Int -> Int) -> ST s (DynArray s e)
emptyWith s gr = do
  arr <- newArray_ (1, s)
  ref <- newSTRef arr
  return $ DynArray ref gr

-- | Read from the array at a given index. If the index has not yet been written
-- to, behavior is undefined.
(!) :: DynArray s e -> Int -> ST s e
arr ! i = do
  a <- readSTRef (payload arr)
  readArray a i

-- | Write to the array at a given index. The array will automatically grow
-- to allow itself to be written at any (postive-valued) index.
wr :: DynArray s e -> Int -> e -> ST s ()
wr arr i x = do
  a <- readSTRef (payload arr)
  (_, sz) <- getBounds a
  if i <= sz
    then writeArray a i x
    else grow arr >> wr arr i x

-- | Grow the capacity of the array according to it's growth strategy.
grow :: DynArray s e -> ST s ()
grow arr = do
  a <- readSTRef (payload arr)
  (_,sz) <- getBounds a
  let sz' = growth arr sz
  a' <- newArray_ (1,sz') 
  writeSTRef (payload arr) a'
  getAssocs a >>= mapM_ (uncurry (writeArray a'))
