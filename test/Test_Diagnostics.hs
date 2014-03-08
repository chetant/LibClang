import System.Environment(getArgs)
import Control.Monad(when)
import Control.Monad.IO.Class(liftIO)
import Data.Maybe(fromJust, isNothing)
import qualified Clang.TranslationUnit as Clang
import qualified Clang.Diagnostic as Diagnostic
import qualified Clang.String as CStr

test tu = do
  diags <- Diagnostic.getDiagnostics tu
  diagDispOpts <- Diagnostic.defaultDisplayOptions
  let printDiag d = Diagnostic.formatDiagnostic diagDispOpts d >>= (liftIO . putStrLn . ("Diag:" ++) . CStr.unpack)
  -- putStrLn $ "numDiags:" ++ show (length diags)
  mapM_ printDiag diags

main = do
  (arg:args) <- getArgs
  Clang.withCreateIndex False False $ \index -> 
      Clang.withParse index (Just arg) args [] [Clang.TranslationUnit_None] test (error "No TXUnit!")
