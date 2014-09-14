{- |
A priority map is a heap fused with a map. It is a mutable data structure.
Its primary advantage is that it allows quick access to the heap, allowing
for quick modification or deletion of elements from the heap.

Complexities mentioned in the documentation are valid if the map that is used
is a tree map.

Values with the /lowest/ score are placed at the top of the heap.

-}

module Data.PriorityMap (
-- * Priority map type
PriorityMap (..), PrioMap,
-- * Query
Data.PriorityMap.null, lookupValue, toList,
-- * Construction
empty, fromList,
-- * Modification
insert, pop, updateValue, updateMapKey, updatePriority, delete,
-- * Internals
-- | These are not part of the API, and may change from version to version.
Info (..), updateCallback
) where

import Data.PriorityMap.Heap (Heap)
import qualified Data.PriorityMap.Heap as H
import Data.PriorityMap.STMap

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad (forM)

import Control.Monad.ST (ST)
import Data.STRef

-- | The default priority map, which uses an STRef to a Data.Map as its
-- map structure.
type PrioMap s = PriorityMap s MapST

data Info km v = Info Int km v deriving Show

-- | A general priority map structure. @s@ is the state thread in the ST monad,
-- @m@ is the map type, @km@ is the keys for the map, @kh@ is the keys for
-- the heap, and @v@ is the value type.
data PriorityMap s m km kh v = PriorityMap
  { pmmap  :: m s km (STRef s (Info km v)) -- ^ the map
  , pmheap :: Heap s kh (STRef s (Info km v)) -- ^ the heap
  }

-- | Create a new empty priority map.
empty :: STMap m => ST s (PriorityMap s m km kh v)
empty = do
  h <- H.empty
  m <- mEmpty
  return (PriorityMap m h)

-- | /O(1)/. Returns true iff the priority map is null.
null :: STMap m => PriorityMap s m km kh v -> ST s Bool
null (PriorityMap m _) = (0 ==) <$> mSize m

updateCallback :: H.OnUpdate s (k, STRef s (Info km v))
updateCallback i (_, ref) = modifySTRef' ref (\(Info _ km v) -> Info i km v)

-- | /O(n log n)/. Create a new priority map from a list of map keys, heap
-- scores, and values. Could be improved in the future to be /O(n)/.
fromList :: (STMap m, Ord km, Ord kh) => [(km, kh, v)] -> ST s (PriorityMap s m km kh v)
fromList xs = do
  m <- empty
  mapM_ (insert m) xs
  return m

-- | /O(n)/. Return a list of the map keys and values of all entries currently
-- in the priority map.
toList :: STMap m => PriorityMap s m km kh v -> ST s [(km, v)]
toList (PriorityMap m _) = do
  xs <- mToList m
  forM xs $ \(k, ref) -> do
    (Info _ _ v) <- readSTRef ref
    return (k, v)

-- | /O(log n)/. Insert a value into the priority map. There should not already
-- be an entry with the given map key. If there is, behavior is undefined,
-- and it is likely that bad things will happen!
insert :: (STMap m, Ord kh, Ord km) => PriorityMap s m km kh v -> (km, kh, v) -> ST s ()
insert (PriorityMap m h) (km, kh, v) = do
  info <- newSTRef (Info undefined km v)
  mInsert m (km, info)
  H.insertWith h updateCallback (kh, info)

-- | /O(log n)/. If the priority map is empty, returns 'Nothing'. If the 
-- priority map is not empty, removes the entry with the lowest score from the
-- heap and returns 'Just' that entry.
pop :: (STMap m, Ord kh, Ord km) => PriorityMap s m km kh v -> ST s (Maybe (km, kh, v))
pop (PriorityMap m h) = do
  res <- H.popWith h updateCallback
  case res of
    Nothing -> return Nothing
    Just (kh, ref) -> do
      (Info _ km v) <- readSTRef ref
      mDelete m km
      return $ Just (km, kh, v)

-- | /O(log n)/. Checks whether there is an element in the priority map with
-- the given map key; if there is not, returns 'Nothing', and if there is,
-- returns 'Just' the value associated with that key.
lookupValue :: (STMap m, Ord km) => PriorityMap s m km kh v -> km -> ST s (Maybe v)
lookupValue (PriorityMap m _) k = do
  r <- mLookup m k
  case r of 
    Nothing -> return Nothing
    Just ref -> do
      (Info _ _ v) <- readSTRef ref
      return $ Just v

-- | /O(log n)/. Modifies the value associated to a given map key. If no entry
-- currently exists for that key, behavior is undefined. Currently, an exception
-- should be raised.
updateValue :: (STMap m, Ord km) => PriorityMap s m km kh v -> km -> (v -> ST s v) -> ST s ()
updateValue (PriorityMap m _) km f = do
  Just ref <- mLookup m km
  Info i km v <- readSTRef ref
  v' <- f v
  writeSTRef ref $! Info i km v'

-- | /O(log n)/. Modifies the map key of an entry. If no entry
-- currently exists for that key, behavior is undefined. Currently, an exception
-- should be raised.
updateMapKey :: (STMap m, Ord km) => PriorityMap s m km kh v -> km -> km -> ST s ()
updateMapKey (PriorityMap m _) km km' = do
  Just ref <- mLookup m km
  mDelete m km
  mInsert m (km', ref)

-- | /O(log n)/. Delete an entry. If no entry
-- currently exists for that key, behavior is undefined. Currently, an exception
-- should be raised.
delete :: (STMap m, Ord km, Ord kh) => PriorityMap s m km kh v -> km -> ST s ()
delete (PriorityMap m h) km = do
  Just ref <- mLookup m km
  mDelete m km
  (Info i _ _) <- readSTRef ref
  (kh, ref') <- H.lookup h i
  H.deleteWith h updateCallback i

-- | /O(log n)/. Update the priority for a given entry. If no entry
-- currently exists for that key, behavior is undefined. Currently, an exception
-- should be raised.
updatePriority :: (STMap m, Ord km, Ord kh) => PriorityMap s m km kh v -> km -> (v -> kh) -> ST s ()
updatePriority (PriorityMap m h) km f = do
  Just ref <- mLookup m km
  (Info i km v) <- readSTRef ref
  H.modifyWith h updateCallback i (first $ const (f v))
