import System.Environment(getArgs)
import Control.Applicative((<$>))
import Clang.TranslationUnit(withCreateIndex, withParse, TranslationUnitFlags(..), getCursor)
import Clang.Traversal(ChildVisitor, visitChildren, ChildVisitResult(..))
import qualified Clang.Type as Type
import qualified Clang.Cursor as Cursor
import Clang.Alloc.Storable

visitor :: String -> ChildVisitor Int
visitor prefix c p d = do
  let nameString = "Name :" ++ show (Cursor.getDisplayName c)
  let typeString = "Kind :" ++ show (Cursor.getCursorKindSpelling . Cursor.getKind $ c)
  putStrLn $ prefix ++ nameString ++ " " ++ typeString 
  case  (Cursor.getKind c) of
    Cursor.Cursor_ClassDecl -> visitChildren c (visitor " - ") Nothing 
    Cursor.Cursor_CXXMethod -> visitChildren c (visitor " - ") Nothing                            
    _ -> return (d, False)
  return (d, ChildVisit_Continue)

test tu = visitChildren (getCursor tu) (visitor " ")  Nothing

main = do
  (arg:args) <- getArgs
  withCreateIndex False False $ \index -> 
      withParse index (Just arg) args [] [TranslationUnit_None] test (error "No TXUnit!")
