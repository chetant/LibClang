import System(getArgs)
import Control.Applicative((<$>))
import Clang.TranslationUnit(withCreateIndex, withParse, TranslationUnitFlags(..), getCursor)
import Clang.Traversal(ChildVisitor, visitChildren, ChildVisitResult(..))
import qualified Clang.Type as Type
import qualified Clang.Cursor as Cursor
import Clang.Alloc.Storable

visitor :: ChildVisitor Int
visitor c p d = do
  putStrLn $ "Type:" ++ show (Type.getTypeKindSpelling . Type.getKind . Cursor.getType $ c)
  return (d, ChildVisit_Continue)

test tu = visitChildren (getCursor tu) visitor Nothing

main = do
  (arg:args) <- getArgs
  withCreateIndex False False $ \index -> 
      withParse index (Just arg) args [] [TranslationUnit_None] test (error "No TXUnit!")
