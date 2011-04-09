import System(getArgs)
import Data.Maybe(fromJust, isJust)
import qualified Clang.TranslationUnit as Clang
import qualified Clang.Diagnostic as Diagnostic

main = do
  args <- getArgs
  index <- Clang.createIndex False False
  mtu <- Clang.parse index Nothing args [] [Clang.TranslationUnit_None]
  let tu = if isJust mtu then fromJust mtu else error "No TXUnit!"
      diags = Diagnostic.getDiagnostics tu
      printDiag d = Diagnostic.formatDiagnostic Diagnostic.getDefaultDisplayOptions d >>=
                  putStrLn . ("Diag:" ++)
  putStrLn $ "numDiags:" ++ show (length diags)
  mapM_ printDiag diags
