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
data SuffixTree c e = SuffixTree (Maybe e) (Map.Map c (SuffixTree c e)) deriving(Show,Eq,Ord)

empty = SuffixTree Nothing Map.empty
singleton v = SuffixTree (Just v) Map.empty

insert [] v (SuffixTree _ mp) = SuffixTree (Just v) mp
insert (x:xs) v (SuffixTree v' mp) =
  case Map.lookup x mp of
    Nothing -> let newSuffixTree = insert xs v empty
                   newMp = Map.insert x newSuffixTree mp
               in SuffixTree v' newMp
    Just tree -> let newSuffixTree = insert xs v tree
                     newMp = Map.insert x newSuffixTree mp
                 in SuffixTree v' newMp

lookup [] (SuffixTree v _) = v
lookup (x:xs) (SuffixTree _ mp) =
  case Map.lookup x mp of
    Nothing -> Nothing
    Just tree -> SuffixTree.lookup xs tree

delete [] (SuffixTree _ mp)
  | Map.null mp = empty
  | otherwise = SuffixTree Nothing mp
delete (x:xs) a@(SuffixTree v mp) =
  case Map.lookup x mp of
    Nothing -> a
    Just tree ->
      let newSuffixTree = delete xs tree
      in if newSuffixTree == empty
         then (SuffixTree v (Map.delete x mp))
         else (SuffixTree v (Map.insert x newSuffixTree mp))
        
instance Foldable (SuffixTree c) where
  foldMap f (SuffixTree mv mp) =
    let currMonoid = case mv of Nothing -> mempty
                                Just v -> f v
    in if Map.null mp
       then currMonoid
       else currMonoid `mappend` (foldMap (foldMap f) mp)
