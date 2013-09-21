reverse [] = []
reverse (x:xs) = rev [x] xs where
  rev acc [] = acc
  rev acc (y:ys) = rev (y:acc) ys