import System(getArgs)
import Control.Monad(when)
import Data.Maybe(fromJust, isNothing)
import qualified Clang.TranslationUnit as Clang
import qualified Clang.Diagnostic as Diagnostic

main = do
  (arg:args) <- getArgs
  index <- Clang.createIndex False False
  mtu <- Clang.parse index (Just arg) args [] [Clang.TranslationUnit_None]
  when (isNothing mtu) $ error "No TXUnit!"
  let diags = Diagnostic.getDiagnostics $ fromJust mtu
      printDiag d = Diagnostic.formatDiagnostic Diagnostic.getDefaultDisplayOptions d >>=
                    putStrLn . ("Diag:" ++)
  -- putStrLn $ "numDiags:" ++ show (length diags)
  mapM_ printDiag diags
