import System.Environment(getArgs)
import Text.Printf

import Control.Applicative((<$>))
import Clang.TranslationUnit(withCreateIndex, withParse, TranslationUnitFlags(..), getCursor)
import Clang.Traversal(ChildVisitor, visitChildren, ChildVisitResult(..))
import qualified Clang.Type as Type
import qualified Clang.Cursor as Cursor
import Clang.Alloc.Storable

whitespace l = concat (replicate l " ")
visitor :: String -> ChildVisitor Int
visitor prefix cursor parent usrData = do
  let cKind = Cursor.getKind cursor
  let nameString = show (Cursor.getDisplayName cursor)
  let kindString = show (Cursor.getCursorKindSpelling cKind)
  printf "Name:%s, Kind:%s\n" nameString kindString
  -- case cKind of
  --   Cursor.Cursor_ClassDecl -> visitChildren cursor (visitor $ (whitespace (length prefix + 2)) ++ "-") Nothing 
  --   Cursor.Cursor_CXXMethod -> visitChildren cursor (visitor $ (whitespace (length prefix + 2)) ++ "-") Nothing
  --   _ -> return (usrData, False)
  return (usrData, ChildVisit_Continue)

test tu = visitChildren (getCursor tu) (visitor " ")  Nothing

main = do
  (arg:args) <- getArgs
  withCreateIndex False False $ \index -> 
      withParse index (Just arg) args [] [TranslationUnit_None] test (error "No TXUnit!")
