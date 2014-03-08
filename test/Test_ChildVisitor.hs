import System.Environment(getArgs)
import Data.IORef
import Data.List
import Data.Typeable
import Text.Printf
import System.IO

import Control.Applicative
import Clang.TranslationUnit
import Clang.Traversal
import Clang.Monad
import qualified Clang.String as String
import qualified Clang.Type as Type
import qualified Clang.Cursor as Cursor
import qualified Clang.Source as Source
import qualified Clang.File as File
import Clang.File
import qualified Clang.CrossReference as XRef
import System.FilePath.Posix
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class

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

kidVisitor :: IORef [DefInfo] -> ChildVisitor s
kidVisitor dsRef cursor _ = do
  cKind <- Cursor.getKind cursor
  loc <- Cursor.getLocation cursor
  (f, ln, col, _) <- Source.getSpellingLocation loc
  file <- case f of
               Just validF -> File.getName validF >>= String.unpack
               Nothing     -> return ""
  --when (inProject file && isDef cursor cKind) $ do
  --isItDef <- isDef cursor cKind
  let isItDef = True
  when (inProject file && isItDef) $ do
    usr <- XRef.getUSR cursor >>= String.unpack
    name <- fqn cursor
    kind <- Cursor.getCursorKindSpelling cKind >>= String.unpack
    -- putStrLn $ "Name: " ++ usr
    -- putStrLn $ "Kind: " ++ kind
    -- putStrLn $ "File: " ++ (normalise file)
    -- putStrLn $ "Line" ++ (show ln)
    -- putStrLn $ "Col" ++ (show col)
    -- putStrLn $ "Usr: " ++ usr
    def <- return $ DefInfo (Identifier name usr)
                      (SourceLocation (normalise file) ln col)
                      kind
    defEvaled <- liftIO $ evaluate def
    liftIO $ modifyIORef dsRef $! (defEvaled :)
  let next = case cKind of
              Cursor.Cursor_FunctionDecl -> ChildVisit_Continue
              Cursor.Cursor_CXXMethod    -> ChildVisit_Continue
              _                          -> ChildVisit_Recurse
  return next

-- kidVisitor = undefined

inProject :: FilePath -> Bool
inProject = isRelative .&&. isValid .&&. (not . null)

isDef :: Cursor.Cursor -> Cursor.CursorKind -> ClangApp s Bool
isDef c k = do
  q1 <- Cursor.isDefinition c
  return $ q1 && not (k == Cursor.Cursor_CXXAccessSpecifier)

fqn :: Cursor.Cursor -> ClangApp s String
fqn cr = (intercalate "::" . reverse) <$> go cr
  where go c = do
          nc <- Cursor.isNullCursor c
          if nc then return []
            else do
              k <- Cursor.getKind c
              isT <- Cursor.isTranslationUnit k
              if isT then return []
                else do
                  parent <- Cursor.getSemanticParent c
                  pl <- go parent
                  dn <- Cursor.getDisplayName c
                  dns <- String.unpack dn
                  return $ dns : pl

test :: IORef [DefInfo] -> TranslationUnit -> ClangApp s ()
test resultsRef tu = do
    cursor <- getCursor tu
    visitChildren cursor (kidVisitor resultsRef)
    items <- liftIO $ readIORef resultsRef
    mapM_ snarf items
  where
    snarf i = liftIO $ do
      print i
      hFlush stdout

main = do
  (arg:args) <- getArgs
  resultsRef <- newIORef [] :: IO (IORef [DefInfo])
  withCreateIndex False False $ \index -> 
      withParse index (Just arg) args [] [TranslationUnit_None] (test resultsRef) (error "No TXUnit!")
