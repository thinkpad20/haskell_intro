
data Vertex = Vertex String [Vertex]

a, b, c, d, e, f, g :: Vertex
a = Vertex "a" [b,c] 
b = Vertex "b" [d,e,f] 
c = Vertex "c" [g] 
d = Vertex "d" [] 
e = Vertex "e" [] 
f = Vertex "f" [] 
g = Vertex "g" [] 

visit :: String -> IO ()
visit n = putStrLn $ "visited " ++ n

bfs :: Vertex -> [Vertex] -> IO ()
bfs (Vertex name []) [] = visit name
bfs (Vertex name (a:as)) [] = visit name >> bfs a as
bfs (Vertex name adj) (v:vs) = visit name >> bfs v (vs ++ adj)

main = bfs a []