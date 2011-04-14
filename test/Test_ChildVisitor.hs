import System(getArgs)
import Data.Maybe(maybe)
import Control.Monad((<=<))
import Control.Applicative((<$>))
import Clang.TranslationUnit(createIndex, parse, TranslationUnitFlags(..))
import Clang.Traversal(visitChildren, ChildVisitResult(..))
import Clang.Cursor(getTranslationUnitCursor)
import Clang.Traversal(ChildVisitor, ChildVisitResult(..))
import qualified Clang.FFI as FFI
import Foreign.Ptr(nullPtr)

visitor :: ChildVisitor
visitor c p d = do
  tk <- FFI.getTypeKind <$> FFI.getCursorType c
  str <- (FFI.getCString <=< FFI.getTypeKindSpelling) tk
  putStrLn $ "Type:" ++ str
  return ChildVisit_Continue

main = do
  (arg:args) <- getArgs
  index <- createIndex False False
  tu <- maybe (error "No TXUnit!") id <$>
        parse index (Just arg) args [] [TranslationUnit_None]
  visitChildren (getTranslationUnitCursor tu) visitor nullPtr
