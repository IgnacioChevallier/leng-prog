module Shape where

data Point = Point { x::Double, y:: Double} deriving (Eq, Show)

data Circle    = Circle    Point Double deriving (Eq, Show)
data Rectangle = Rectangle Point Point deriving (Eq, Show)


-- A point from a tuple Pair
point::(Double, Double) -> Point
point (n, m) = (Point n m)

-- The origin
origin::Point
origin = Point 0 0

-- Rectangle from a Tuple where (x0 y0) == origin
rectangle::(Double, Double) -> Rectangle
rectangle (a, b) = (Rectangle origin (Point a b))

base::Rectangle -> Double
base (Rectangle (Point _ _) (Point w _))  = w

height::Rectangle -> Double
height (Rectangle (Point _ _) (Point _ z)) = z

-- Circle from radius
circle::Double -> Circle
circle rad = Circle origin rad

-- Clase Shift

class Shift a where
   shift::a -> (Double, Double) -> a
   
instance Shift Point where
   shift (Point x1 y1) (dx, dy) = Point (x1 + dx) (y1 + dy)

instance Shift Rectangle where
   shift (Rectangle (Point x1 y1) (Point x2 y2)) (dx, dy) = Rectangle (Point (x1 + dx) (y1 + dy)) (Point (x2 + dx) (y2 + dy))

instance Shift Circle where
   shift (Circle (Point x1 y1) r) (dx, dy) = Circle (Point (x1 + dx) (y1 + dy)) r

-- Define the Surface class
   
class Surface a where
   surface::a -> Double

instance Surface Circle where
    surface (Circle _ r) = pi * r * r

instance Surface Rectangle where
    surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)