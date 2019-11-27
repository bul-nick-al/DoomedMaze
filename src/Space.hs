{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Space where

-- | Координаты ячейки в дискретном пространстве.
type Coords = (Int, Int)

-- | Прямоугольная область в дискретном пространстве,
-- заданная координатами противоположных вершин.
data Area = Area Coords Coords
  deriving (Show)

-- | Построить наименьшую прямоугольную область,
-- захватывающую две заданные области.
maxArea :: Area -> Area -> Area
maxArea (Area (l1, b1) (r1, t1)) (Area (l2, b2) (r2, t2))
  = Area (min l1 l2, min b1 b2) (max r1 r2, max t1 t2)

-- | Проверить, что точка находится в заданной области.
inArea :: Area -> Coords -> Bool
inArea (Area (l, b) (r, t)) (i, j)
    = l <= i && i <= r
  && b <= j && j <= t

-- | Пространство с объектами.
data Space a = Space
  { spaceObjects :: [(Coords, a)] -- ^ Объекты в пространстве.
  , spaceArea    :: Maybe Area    -- ^ Границы пространства.q
  } deriving (Show, Functor, Foldable)


instance Semigroup(Space a) where
    (<>) = union

-- | Пространства образуют моноид с операцией 'union'.
instance Monoid (Space a) where 
  mempty  = empty
  mconcat  (x:xs) = x <> mconcat xs

-- | Пустое пространство.
empty :: Space a
empty = Space [] Nothing

-- | Пространство с одним объектом.
singleton :: Coords -> a -> Space a
singleton point x = Space [(point, x)] (Just (Area point point))

-- | Объединение двух пространств @'Space' a@.
union :: Space a -> Space a -> Space a
union (Space xs ax) (Space ys ay) = Space (xs ++ ys) (maxArea' ax ay)
  where
    maxArea' Nothing a2 = a2
    maxArea' a1 Nothing = a1
    maxArea' (Just a1) (Just a2) = Just (maxArea a1 a2)

-- | Построить пространство из списка объектов.
fromList :: [(Coords, a)] -> Space a
fromList = foldMap (uncurry singleton)

fromCoordsList :: [Coords] -> Space String 
fromCoordsList = fromList . map delta

crop :: Area -> Space a -> Space a
crop area space = Space
  { spaceObjects = filter (inArea area . fst) (spaceObjects space)
  , spaceArea    = Just area
  }

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

delta :: a -> (a, String)
delta x = (x, "|")

transposeArea :: Area -> Area
transposeArea (Area lb rt) = Area (swap lb) (swap rt)

transposeSpace :: Space a -> Space a
transposeSpace (Space objects area) = Space
  (fmap (\(coords, x) -> (swap coords, x)) objects)
  (fmap transposeArea area)

splitH :: Int -> Area -> (Area, Area)
splitH x (Area (l, b) (r, t)) = (left, right)
  where
    left  = Area (l, b) (x - 1, t)
    right = Area (x + 1, b) (r, t)

thinArea :: Area -> Bool
thinArea (Area (l, b) (r, t)) = r <= l + 1 || t <= b + 1
    

generateIndexRange :: Area -> [Coords]
generateIndexRange (Area (x1, y1) (x2,y2)) = 
    [(i,j) | i <- [(-x1)..x2], j <- [(-y1)..y2]]


