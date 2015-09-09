import System.Environment(getArgs)
import Control.Monad.IO.Class(liftIO)
import Clang.String(unpack)
import Clang.TranslationUnit(getDiagnosticSet)
import Clang(parseSourceFile)
import qualified Clang.Diagnostic as Diagnostic(format, getElements)

test tu = do getDiagnosticSet tu >>= Diagnostic.getElements >>= mapM_ printDiag
    where printDiag d = Diagnostic.format Nothing d >>=
                        unpack >>=
                        (liftIO . putStrLn . ("Diag:" ++))

main = do
  (arg:args) <- getArgs
  parseSourceFile arg args test
