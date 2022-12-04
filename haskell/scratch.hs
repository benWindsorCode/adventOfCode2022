main :: IO ()
main = do
    let x = Rectangle 10 20 20 10
    print $ surface x

data Shape = Circle Float Float Float | Rectangle Float Float Float Float

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r * r
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x2) * (abs $ y2 - y1)