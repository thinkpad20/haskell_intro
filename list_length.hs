-- not tail-recursive
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- tail-recursive
length'' :: [a] -> Int
length'' list = len 0 list where
   len acc [] = acc
   len acc (x:xs) = len (acc + 1) xs

-- not tail-recursive
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

--reverse' "abc" = "bca"
--   (reverse' "bc") ++ "a" = "bca"
--      (reverse' "c") ++ "b" = "bc"
--         (reverse' "") ++ "c" = "c"

-- tail-recursive
reverse'' :: [a] -> [a]
reverse'' list = rev [] list where
  rev acc [] = acc
  rev acc (x:xs) = rev (x:acc) xs

--reverse'' "abc" = rev "" "abc" = "cba"
--   = rev "a" "bc"
--      = rev "ba" "c"
--         = rev "cba" "" = "cba"
