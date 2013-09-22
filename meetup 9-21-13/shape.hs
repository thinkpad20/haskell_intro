data Shape = Circle Double
           | Rectangle Double Double
           deriving Show
           
area (Circle radius) = pi * radius ^ 2
area (Rectangle l w) = l * w