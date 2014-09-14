{- |

A typeclass which represents mutable key-value map data structures in the ST
monad. Keys must be in the 'Ord' typeclass.
-}

module Data.PriorityMap.STMap where

import Control.Applicative ((<$>))
import Control.Monad.ST (ST)
import Data.STRef

import Data.Map (Map)
import qualified Data.Map as M

class STMap m where
  mEmpty :: ST s (m s k v)
  mSize :: m s k v -> ST s Int
  mModify :: Ord k => m s k v -> k -> Maybe v -> ST s ()
  mLookup :: Ord k => m s k v -> k -> ST s (Maybe v)
  mToList :: m s k v -> ST s [(k, v)]

  mDelete :: Ord k => m s k v -> k -> ST s ()
  mDelete m k = mModify m k Nothing

  mInsert :: Ord k => m s k v -> (k, v) -> ST s ()

newtype MapST s k v = MapST (STRef s (Map k v))

modify :: MapST s k v -> (Map k v -> Map k v) -> ST s ()
modify (MapST ref) f = modifySTRef' ref f

get :: MapST s k v -> (Map k v -> a) -> ST s a
get (MapST ref) f = f <$> readSTRef ref

instance STMap MapST where
  mEmpty = MapST <$> newSTRef M.empty
  mSize m = get m M.size
  mModify m k val = modify m (M.update (const val) k)
  mLookup m k = get m (M.lookup k)
  mToList m = get m M.toAscList
  mInsert m (k, v) = modify m (M.insert k v)
