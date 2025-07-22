module ZykovCore (Graph, eval) where
-- contenido reducido desde ZykovLang3.hs
-- solo las funciones algebraicas y parser

type Graph a = ([a], [[a]])

-- definición de v, o, .+, .*, kom, etc.

-- parser: eval :: String -> Graph String