import System(getArgs)
import Data.Maybe(fromJust, isJust)
import qualified Clang.FFI as Clang
-- import qualified Clang.TranslationUnit as Clang
-- import qualified Clang.Diagnostic as Diagnostic

main = do
  args <- getArgs
  index <- Clang.createIndex False False
  mtu <- Clang.parseTranslationUnit index Nothing args [] 0
  let tu = if isJust mtu then fromJust mtu else error "No TXUnit!"
  defDisplayOpts <- Clang.defaultDiagnosticDisplayOptions
  let printDiag i = do
         diag <- Clang.getDiagnostic tu i
         clstr <- Clang.formatDiagnostic diag defDisplayOpts
         str <- Clang.getCString clstr
         putStrLn $ "Diag:" ++ str
  numDiags <- Clang.getNumDiagnostics tu
  putStrLn $ "numDiags:" ++ show numDiags
  mapM_ printDiag [0..numDiags]
