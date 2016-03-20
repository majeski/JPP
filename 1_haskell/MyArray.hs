module MyArray
( Ix (..)
, Array
, contains
, (!)
, elems
, listArray
, array
, update
, (//)
) where

class Ord a => Ix a where
  range :: (a, a) -> [a]
  index :: (a, a) -> a -> Int
  index r x
    | inRange r x = unsafeIndex r x
    | otherwise = error "index out of bounds"
  unsafeIndex :: (a, a) -> a -> Int
  inRange :: (a, a) -> a -> Bool
  rangeSize :: (a, a) -> Int

instance Ix Integer where
  range (lo, hi) = [lo..hi]
  unsafeIndex (lo, _) x = fromInteger $ x - lo
  inRange (lo, hi) x = lo <= x && x <= hi
  rangeSize (lo, hi) = fromInteger $ max 0 (hi - lo + 1)

instance Ix Int where
  range (lo, hi) = [lo..hi]
  unsafeIndex (lo, _) x = x - lo
  inRange (lo, hi) x = lo <= x && x <= hi
  rangeSize (lo, hi) = max 0 (hi - lo + 1)

instance Ix Char where
  range (lo, hi) = [lo..hi]
  unsafeIndex (lo, hi) x = unsafeIndex (fromEnum lo, fromEnum hi) (fromEnum x)
  inRange (lo, hi) x = lo <= x && x <= hi
  rangeSize (lo, hi) = rangeSize (fromEnum lo, fromEnum hi)

instance (Ix a, Ix b) => Ix (a, b) where
  range ((alo, blo), (ahi, bhi)) = [(x, y) | x <- range (alo, ahi), y <- range (blo, bhi)]
  unsafeIndex ((alo, blo), (ahi, bhi)) (ax, bx) = 
    (index (alo, ahi) ax) * rangeSize (blo, bhi) + index (blo, bhi) bx
  inRange ((alo, blo), (ahi, bhi)) (ax, bx) = inRange (alo, ahi) ax && inRange (blo, bhi) bx
  rangeSize ((alo, blo), (ahi, bhi)) = rangeSize (alo, ahi) * rangeSize (blo, bhi)

data Array i e = Array (Root e) (i, i)
type Root e = (Tree e, (Int, Int))
data Tree e =  Empty | Leaf e | Node (Tree e) (Tree e)

contains :: Ix i => Array i e -> i -> Bool
contains (Array root r) i = case get root (index r i) of
  Just _ -> True
  Nothing -> False

(!) :: Ix i => Array i e -> i -> e
(!) (Array root r) i = case get root (index r i) of
  Just val -> val
  Nothing -> error "there is no element for given index"

get :: Root e -> Int -> Maybe e
get (Empty, _) _ = Nothing
get (Leaf val, _) _ = Just val
get (Node lc rc, r) idx
  | idx <= mid r = get (lc, leftRange r) idx
  | otherwise = get (rc, rightRange r) idx

leftRange :: (Int, Int) -> (Int, Int)
leftRange r@(x, _) = (x, mid r)

rightRange :: (Int, Int) -> (Int, Int)
rightRange r@(_, y) = (mid r + 1, y)

mid :: (Int, Int) -> Int
mid (x, y) = fromInteger $ (toInteger x + toInteger y) `div` 2

elems :: Ix i => Array i e -> [e]
elems (Array (tree, _) _) = getAll tree [] where
  getAll Empty acc = acc
  getAll (Leaf val) acc = val : acc
  getAll (Node l r) acc = getAll l (getAll r acc)

listArray :: (Ix i) => (i, i) -> [e] -> Array i e
listArray r values = array r (zip (range r) values)

array :: (Ix i) => (i, i) -> [(i, e)] -> Array i e
array r values = empty // values where
  empty = Array (Empty, treeR) r
  treeR = (0, rangeSize r - 1)

(//) :: (Ix i) => Array i e -> [(i, e)] -> Array i e
(//) = foldl (flip $ uncurry update)

update :: Ix i => i -> e -> Array i e -> Array i e
update i val (Array root@(_, treeRange) r)
  | inRange r i = Array (newTree, treeRange) r
  | otherwise = error "index out of bounds"
  where
    newTree = set root (index r i) val

set :: Root e -> Int -> e -> Tree e
-- change value in leaf
set (Leaf _, _) _ val = Leaf val
-- create node
set (Empty, r) idx val
  | rangeSize r == 1 = Leaf val
  | otherwise = set (Node Empty Empty, r) idx val 
-- choose the way down
set (Node lc rc, r) idx val
  | idx <= mid r = Node (set (lc, leftRange r) idx val) rc
  | otherwise = Node lc (set (rc, rightRange r) idx val)