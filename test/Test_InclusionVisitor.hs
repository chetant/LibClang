import System(getArgs)
import Control.Monad(when)
import Data.Maybe(fromJust, isNothing)
import qualified Clang.FFI as FFI
import qualified Clang.TranslationUnit as Clang
import qualified Clang.Source as Clang
import Foreign.Ptr(nullPtr)

printInclusions f sls p = do
  name <- FFI.getFileName f
  putStrLn $ "Included:" ++ show name

test tu = FFI.getInclusions tu printInclusions nullPtr

main = do
  (arg:args) <- getArgs
  Clang.withCreateIndex False False $ \index -> 
      Clang.withParse index (Just arg) args [] [Clang.TranslationUnit_None] test (error "No TXUnit!")
