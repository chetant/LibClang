import System.Environment(getArgs)
import Control.Monad.IO.Class(liftIO)
import Clang.String(unpack)
import Clang.TranslationUnit(withCreateIndex, withParse, TranslationUnitFlags(..))
import Clang.Diagnostic(getDiagnostics, defaultDisplayOptions, formatDiagnostic)
import qualified Data.Vector as DV(empty)

test tu = do
  diags <- getDiagnostics tu
  diagDispOpts <- defaultDisplayOptions
  let printDiag d = formatDiagnostic (Just diagDispOpts) d >>= 
                    unpack >>= 
                    (liftIO . putStrLn . ("Diag:" ++))
  -- putStrLn $ "numDiags:" ++ show (length diags)
  mapM_ printDiag diags
  return ()

main = do
  (arg:args) <- getArgs
  withCreateIndex False False $ \index -> 
      withParse index (Just arg) args DV.empty [TranslationUnit_None] test (error "No TXUnit!")
