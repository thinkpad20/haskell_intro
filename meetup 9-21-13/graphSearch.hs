newtype Vertex = Vertex (String, [Vertex]) deriving (Eq)

a, b, c, d, e, f, g :: Vertex
a = Vertex ("a", [b,c])
b = Vertex ("b", [d,e,f]) 
c = Vertex ("c", [g] )
d = Vertex ("d", []) 
e = Vertex ("e", []) 
f = Vertex ("f", []) 
g = Vertex ("g", []) 

instance Show Vertex where
  show (Vertex (name, [])) = show name
  show (Vertex (name, vs)) = "(" ++ show name ++ "->" ++ show vs ++ ")"

{- The above nodes define a graph like this:

     a
    / \
   b   c
  /|\   \
 d e f   g

-}

visit :: Vertex -> IO ()
visit v = putStrLn $ "\tvisited " ++ show v

bfs :: (Vertex -> IO ()) -> Vertex -> IO ()
bfs fun v = loop fun v [] where
  loop f v@(Vertex (name, [])) [] = f v
  loop f v@(Vertex (name, (a:as))) [] = f v >> loop f a as
  loop f v@(Vertex (name, adj)) (a:as) = f v >> loop f a (as ++ adj)

dfs :: (Vertex -> IO ()) -> Vertex -> IO ()
dfs f v@(Vertex (_, adj)) = f v >> loop adj where
  loop [] = return ()
  loop (a:as) = dfs f a >> loop as

main = do
  putStrLn "BFS:" >> bfs visit a
  putStrLn "DFS:" >> dfs visit a