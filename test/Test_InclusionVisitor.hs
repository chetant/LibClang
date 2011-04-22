import System(getArgs)
import Control.Monad(when)
import Data.Maybe(fromJust, isNothing)
import Clang.TranslationUnit(withCreateIndex, withParse, TranslationUnitFlags(..))
import qualified Clang.Source as Clang
import Clang.Traversal
import Clang.File(getName)
import Clang.Alloc.Storable

printInclusions :: InclusionVisitor Int
printInclusions f sls d = do
  let name = getName f
  putStrLn $ "Included:" ++ name
  return d

test tu = getInclusions tu printInclusions Nothing

main = do
  (arg:args) <- getArgs
  withCreateIndex False False $ \index -> 
      withParse index (Just arg) args [] [TranslationUnit_None] test (error "No TXUnit!")
