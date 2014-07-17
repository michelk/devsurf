module Main where
import System.FilePath.Posix
import System.Environment
import DevSurf.Readers.Sms2dm
import DevSurf.Writers.ShewTriangle

main :: IO ()
main = do
  args <- getArgs
  let f = head args
  s <- readFile f
  let m = smsMsh . parse2dm $ s
      s_out = toShewTriangle m
      f_base = takeBaseName f
      f_ele = f_base ++ ".ele"
      f_nd = f_base ++ ".node"
  writeFile f_nd (fst s_out)
  writeFile f_ele (snd s_out)
  putStrLn $  "Created " ++ f_nd ++ " and " ++ f_ele
