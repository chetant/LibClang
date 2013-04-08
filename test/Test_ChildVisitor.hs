import System.Environment(getArgs)
import Data.IORef
import Data.List
import Data.Typeable
import Text.Printf
import System.IO

import Control.Applicative
import Clang.TranslationUnit
import Clang.Traversal
import qualified Clang.Type as Type
import qualified Clang.Cursor as Cursor
import Clang.Alloc.Storable
import Clang.File
import qualified Clang.CrossReference as XRef
import qualified Clang.Cursor as C
import qualified Clang.FFI as FFI
import System.FilePath.Posix
import Control.Monad
import Control.Exception

{-
whitespace l = concat (replicate l " ")
visitor :: IORef [String] -> ChildVisitor Int
visitor r cursor parent usrData = do
  let cKind = Cursor.getKind cursor
  let nameString = show (Cursor.getDisplayName cursor)
  let kindString = show (Cursor.getCursorKindSpelling cKind)
  --printf "Name:%s, Kind:%s\n" nameString kindString
  -- case cKind of
  --   Cursor.Cursor_ClassDecl -> visitChildren cursor (visitor $ (whitespace (length prefix + 2)) ++ "-") Nothing 
  --   Cursor.Cursor_CXXMethod -> visitChildren cursor (visitor $ (whitespace (length prefix + 2)) ++ "-") Nothing
  --   _ -> return (usrData, False)
  modifyIORef r $! ((nameString ++ kindString) :)
  return (usrData, ChildVisit_Continue)
-}

data Identifier = Identifier String String deriving (Eq, Show)
data SourceLocation = SourceLocation String Int Int deriving (Eq, Show)
data DefInfo = DefInfo Identifier SourceLocation String deriving (Eq, Show)

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) f g v = f v && g v

kidVisitor :: IORef [DefInfo] -> ChildVisitor Bool
kidVisitor dsRef cursor _ usrData = do
  cKind <- FFI.getCursorKind cursor
  loc <- FFI.getCursorLocation cursor
  (f, ln, col, _) <- FFI.getSpellingLocation loc
  file <- case f of
               Just validF -> FFI.getFileName validF >>= return . show
               Nothing     -> return ""
  --when (inProject file && isDef cursor cKind) $ do
  --isItDef <- isDef cursor cKind
  let isItDef = True
  when (inProject file && isItDef) $ do
    usr <- FFI.getCursorUSR cursor >>= return . show
    name <- fqn cursor
    kind <- FFI.getCursorKindSpelling cKind >>= return . show
    -- putStrLn $ "Name: " ++ usr
    -- putStrLn $ "Kind: " ++ kind
    -- putStrLn $ "File: " ++ (normalise file)
    -- putStrLn $ "Line" ++ (show ln)
    -- putStrLn $ "Col" ++ (show col)
    -- putStrLn $ "Usr: " ++ usr
    def <- return $ DefInfo (Identifier name usr)
                      (SourceLocation (normalise file) ln col)
                      kind
    defEvaled <- evaluate def
    modifyIORef dsRef $! (defEvaled :)
  let next = case cKind of
              C.Cursor_FunctionDecl -> ChildVisit_Continue
              C.Cursor_CXXMethod    -> ChildVisit_Continue
              _                     -> ChildVisit_Recurse
  return (usrData, next)

inProject :: FilePath -> Bool
inProject = isRelative .&&. isValid .&&. (not . null)

isDef :: C.Cursor -> C.CursorKind -> IO Bool
isDef c k = do
  q1 <- FFI.isCursorDefinition c
  return $ q1 && not (k == C.Cursor_CXXAccessSpecifier)

fqn :: C.Cursor -> IO String
fqn cr = (intercalate "::" . reverse) <$> go cr
  where go c = do
          nc <- FFI.getNullCursor
          if c == nc then return []
            else do
              k <- FFI.getCursorKind c
              isT <- FFI.isTranslationUnit k
              if isT then return []
                else do
                  parent <- FFI.getCursorSemanticParent c
                  pl <- go parent
                  dn <- FFI.getCursorDisplayName c
                  dns <- return $ show dn
                  return $ dns : pl

test resultsRef tu = do
    visitChildren (getCursor tu) (kidVisitor resultsRef)  Nothing
    items <- readIORef resultsRef
    mapM_ snarf items
  where
    snarf i = do
      print i
      hFlush stdout


main = do
  (arg:args) <- getArgs
  resultsRef <- newIORef []
  withCreateIndex False False $ \index -> 
      withParse index (Just arg) args [] [TranslationUnit_None] (test resultsRef) (error "No TXUnit!")
