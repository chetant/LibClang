import System(getArgs)
import Data.Maybe(maybe)
import Control.Monad((<=<))
import Control.Applicative((<$>))
import Clang.TranslationUnit(withCreateIndex, withParse, TranslationUnitFlags(..), getCursor)
import Clang.Traversal(ChildVisitor, visitChildren, ChildVisitResult(..))
import qualified Clang.FFI as FFI
import Foreign.Ptr(nullPtr)

visitor :: ChildVisitor Int
visitor c p d = do
  tk <- FFI.getTypeKind <$> FFI.getCursorType c
  str <- (FFI.getCString <=< FFI.getTypeKindSpelling) tk
  putStrLn $ "Type:" ++ str
  return (d, ChildVisit_Continue)

test tu = visitChildren (getCursor tu) visitor Nothing

main = do
  (arg:args) <- getArgs
  withCreateIndex False False $ \index -> 
      withParse index (Just arg) args [] [TranslationUnit_None] test (error "No TXUnit!")
