join [] list2 = list2
join list1 list2 = h : (join t list2) where
  t = tail list1
  h = head list1

s1 = "hello "
s2 = "world"