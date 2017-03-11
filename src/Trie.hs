module Trie
  (empty,
   singleton,
   insert,
   lookup,
   delete)where
import qualified Data.Map.Lazy as Map
import Prelude ();
import Data.Maybe
import Data.Bool
import Text.Show
import Data.Eq
import Data.Ord
import Data.Monoid
import Data.Foldable
data Trie c e = Trie (Maybe e) (Map.Map c (Trie c e)) deriving(Show,Eq,Ord)

empty = Trie Nothing Map.empty
singleton v = Trie (Just v) Map.empty

insert [] v (Trie _ mp) = Trie (Just v) mp
insert (x:xs) v (Trie v' mp) =
  case Map.lookup x mp of
    Nothing -> let newTrie = insert xs v empty
                   newMp = Map.insert x newTrie mp
               in Trie v' newMp
    Just tree -> let newTrie = insert xs v tree
                     newMp = Map.insert x newTrie mp
                 in Trie v' newMp

lookup [] (Trie v _) = v
lookup (x:xs) (Trie _ mp) =
  case Map.lookup x mp of
    Nothing -> Nothing
    Just tree -> Trie.lookup xs tree

delete [] (Trie _ mp)
  | Map.null mp = empty
  | otherwise = Trie Nothing mp
delete (x:xs) a@(Trie v mp) =
  case Map.lookup x mp of
    Nothing -> a
    Just tree ->
      let newTrie = delete xs tree
      in if newTrie == empty
         then (Trie v (Map.delete x mp))
         else (Trie v (Map.insert x newTrie mp))
        
instance Foldable (Trie c) where
  foldMap f (Trie mv mp) =
    let currMonoid = case mv of Nothing -> mempty
                                Just v -> f v
    in if Map.null mp
       then currMonoid
       else currMonoid `mappend` (foldMap (foldMap f) mp)
