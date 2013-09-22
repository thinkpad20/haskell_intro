data List = Empty
          | (:::) Int List
          deriving Show -- generate a print function

--leftEnd = Empty
--left1 = Prepend 5 leftEnd
--left2 = Prepend 6 left1
--left3 = Prepend 100 left2

--rightEnd = Empty
--right1 = Prepend 10 rightEnd
--right2 = Prepend 15 right1
--right3 = Prepend 76 right2


join :: List -> List -> List
join l1 l2 = case l1 of
  Empty -> l2
  (x ::: xs) -> x ::: (join xs l2)

gen :: [Int] -> List
gen [] = Empty
gen (i:is) = i ::: (gen is)