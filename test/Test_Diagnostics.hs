import System.Environment(getArgs)
import Control.Monad(when)
import Data.Maybe(fromJust, isNothing)
import qualified Clang.TranslationUnit as Clang
import qualified Clang.Diagnostic as Diagnostic

test tu = do
  let diags = Diagnostic.getDiagnostics tu
      printDiag d = Diagnostic.formatDiagnostic Diagnostic.getDefaultDisplayOptions d >>=
                    putStrLn . ("Diag:" ++)
  -- putStrLn $ "numDiags:" ++ show (length diags)
  mapM_ printDiag diags

main = do
  (arg:args) <- getArgs
  Clang.withCreateIndex False False $ \index -> 
      Clang.withParse index (Just arg) args [] [Clang.TranslationUnit_None] test (error "No TXUnit!")
