import System(getArgs)
import Data.Maybe(maybe)
import Control.Applicative((<$>))
import Clang.TranslationUnit(createIndex, parse, TranslationUnitFlags(..))
import Clang.Traversal(visitChildren, ChildVisitResult(..))
import Clang.Cursor(getTranslationUnitCursor)
import Clang.Traversal(ChildVisitor, ChildVisitResult(..))
import Foreign.Ptr(nullPtr)

visitor :: ChildVisitor
visitor c p d = do
  print "BOOY"
  return ChildVisit_Continue

main = do
  (arg:args) <- getArgs
  index <- createIndex False False
  tu <- maybe (error "No TXUnit!") id <$>
        parse index (Just arg) args [] [TranslationUnit_None]
  return ()
  visitChildren (getTranslationUnitCursor tu) visitor nullPtr