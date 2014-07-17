module Main where
import Data.List (intersperse)
import Data.HashMap.Lazy (elems)
import DevSurf

main :: IO ()
main = interact smsToBound


smsToBound :: String -> String
smsToBound = unlines . intersperse "\n" . map renderVertices . getBoundaries .  smsMsh . parse2dm
    
     


