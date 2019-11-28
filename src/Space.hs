{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Space where

-- | Data type to represent coordinates
type Coords = (Int, Int)

-- | Rectangle area in discrete space, defined by border coordinates.
data Area = Area Coords Coords
  deriving (Show)

-- | Build smallest rectangle area, uniting 2 given areas. 
maxArea :: Area -> Area -> Area
maxArea (Area (l1, b1) (r1, t1)) (Area (l2, b2) (r2, t2))
  = Area (min l1 l2, min b1 b2) (max r1 r2, max t1 t2)

-- | Check if coords is inside Area 
inArea :: Area -> Coords -> Bool
inArea (Area (l, b) (r, t)) (i, j)
    = l <= i && i <= r
  && b <= j && j <= t

-- | Space with objects
data Space a = Space
  { spaceObjects :: [(Coords, a)] 
  , spaceArea    :: Maybe Area    
  } deriving (Show, Functor, Foldable)


instance Semigroup(Space a) where
    (<>) = union

instance Monoid (Space a) where 
  mempty  = empty
  mconcat  (x:xs) = x <> mconcat xs

-- | Empty space
empty :: Space a
empty = Space [] Nothing

-- | Space with single object
singleton :: Coords -> a -> Space a
singleton point x = Space [(point, x)] (Just (Area point point))

-- | Union of 2 spaces
union :: Space a -> Space a -> Space a
union (Space xs ax) (Space ys ay) = Space (xs ++ ys) (maxArea' ax ay)
  where
    maxArea' Nothing a2 = a2
    maxArea' a1 Nothing = a1
    maxArea' (Just a1) (Just a2) = Just (maxArea a1 a2)

-- | Build space from list of objects
fromList :: [(Coords, a)] -> Space a
fromList = foldMap (uncurry singleton)


-- | Function to swap elements of tuple.
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- | wrapper for basic object
delta :: a -> (a, String)
delta x = (x, "|")

-- | Transpose Area
transposeArea :: Area -> Area
transposeArea (Area lb rt) = Area (swap lb) (swap rt)
-- | Transpose space
transposeSpace :: Space a -> Space a
transposeSpace (Space objects area) = Space
  (fmap (\(coords, x) -> (swap coords, x)) objects)
  (fmap transposeArea area)

-- | Splitting area horizontally
splitH :: Int -> Area -> (Area, Area)
splitH x (Area (l, b) (r, t)) = (left, right)
  where
    left  = Area (l, b) (x - 1, t)
    right = Area (x + 1, b) (r, t)

-- | Check if area is thin.
thinArea :: Area -> Bool
thinArea (Area (l, b) (r, t)) = r <= l + 1 || t <= b + 1
    
-- | Generation of all possible coords for area
generateIndexRange :: Area -> [Coords]
generateIndexRange (Area (x1, y1) (x2,y2)) = 
    [(i,j) | i <- [(-x1)..x2], j <- [(-y1)..y2]]


