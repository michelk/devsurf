module DevSurf.Readers.Utils where
import Control.Applicative
import Data.Attoparsec.Text

float :: Parser Double
float = realToFrac <$> double

int :: Parser Int
int = decimal
