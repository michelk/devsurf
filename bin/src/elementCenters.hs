module Main where
import DevSurf.Face
import DevSurf.Types
import DevSurf.Readers.Sms2dm
import DevSurf.Writers.Xyz

main :: IO ()
main = 
  interact $  renderVertices . map centerFace . elements . smsMsh . parse2dm  
