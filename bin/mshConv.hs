{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import Control.Monad (when)
import Data.Char (toLower)
import DevSurf.Face (elements)
import DevSurf.Convert (fsToIfs)
import DevSurf.Types
import DevSurf.Readers
import DevSurf.Writers
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.FilePath.Posix (takeExtension)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.HashMap.Lazy (elems)

_PROGRAM_NAME    = "mshConv"
_PROGRAM_VERSION = "0.0.0.0"
_PROGRAM_INFO    = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT   = "Convert in between different surface-mesh-file-formats"
_COPYRIGHT       = "GPL licensed; written by Michel Kuhlmann 2013"

-- | Data structure for command line options.
data Opt = Opt
    {
      from   :: String
    , to     :: String
    , output :: String
    --, file   :: FilePath -- ^ Command-line arguments
    } deriving (Show, Data, Typeable)

-- | Defaults for command-line options.
defaultOpts :: Opt
defaultOpts = Opt
    {
      from   = def &= help "Source-format; currently: 2dm (SMS)"
    , to     = def &= help "Target-format; currently: triPoly (triangle .poly file), stl, ply, obj"
    , output = def &= typFile &= help "Output file"
    --, file  = def &= args &= typ "MSH-FILE"
    } &=
    program _PROGRAM_NAME &=
    help _PROGRAM_ABOUT &=
    summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)

-- | Returns a conversion-function based on reader and writer
--   combination
dispatchReaderWriter :: String -> String -> (T.Text -> T.Text)
dispatchReaderWriter reader writer
  | reader == "2dm" && writer == "stl" =
    T.pack . renderSTL .  elements . smsMsh . parse2dm
  | reader == "2dm" && writer == "poly" =
    T.pack . toShewPolyString . triangleMeshToShewPoly . smsMsh . parse2dm
  | reader == "2dm" && writer == "ply" =
    T.pack . renderPly . smsMsh . parse2dm
  | reader == "2dm" && writer == "vtk" =
    T.pack . renderVtk . smsMsh . parse2dm
  | reader == "obj" && writer == "2dm" =
    T.pack . renderSms2dm . parseObj
  | reader == "2dm" && writer == "obj" =
    T.pack . renderObj . smsMsh . parse2dm
  | reader == "2dm" && writer == "str" =
    T.pack . renderDefautlStrPt . elems . ifsVertices . smsMsh . parse2dm
  | reader == "dxf" && writer == "2dm" =
    T.pack . renderSms2dm . fsToIfs . parseDxf
  | otherwise =
    error "reader-writer combination not supported"

main :: IO ()
main = do
    args <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args then withArgs ["--help"] else id) (cmdArgs defaultOpts)
    let f = dispatchReaderWriter (from opts) (to opts)
    TIO.interact f

