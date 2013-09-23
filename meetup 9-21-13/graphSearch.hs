data Vertex a = Vertex (a, [Vertex a])
--data Graph a = 

a, b, c, d, e, f, g :: Vertex String
a = Vertex ("a", [b,c])
b = Vertex ("b", [d,e,f]) 
c = Vertex ("c", [g] )
d = Vertex ("d", []) 
e = Vertex ("e", []) 
f = Vertex ("f", []) 
g = Vertex ("g", []) 

display :: Show a => Vertex a -> String
display (Vertex (a, [])) = show a
display (Vertex (a, vs)) = "(" ++ show a ++ "->" ++ display' vs ++ ")" where
  display' = concat . map display

{- The above nodes define a graph like this:

     a
    / \
   b   c
  /|\   \
 d e f   g

-}

visit :: Show a => Vertex a -> IO ()
visit v = putStrLn $ "\tvisited " ++ display v

bfs :: (Vertex a -> IO ()) -> Vertex a -> IO ()
bfs fun v = loop fun v [] where
  loop f v@(Vertex (name, [])) [] = f v
  loop f v@(Vertex (name, (a:as))) [] = f v >> loop f a as
  loop f v@(Vertex (name, adj)) (a:as) = f v >> loop f a (as ++ adj)

dfs :: (Vertex a -> IO ()) -> Vertex a -> IO ()
dfs f v@(Vertex (_, adj)) = f v >> loop adj where
  loop [] = return ()
  loop (a:as) = dfs f a >> loop as

main = do
  putStrLn "BFS:" >> bfs visit a
  putStrLn "DFS:" >> dfs visit a