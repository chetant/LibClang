import System(getArgs)
import Control.Monad(when)
import Data.Maybe(fromJust, isNothing)
import qualified Clang.FFI as FFI
import qualified Clang.TranslationUnit as Clang
import qualified Clang.Source as Clang
import Foreign.Ptr(nullPtr)

printInclusions f sls p = do
  name <- FFI.getFileName f
  print name

main = do
  args <- getArgs
  index <- Clang.createIndex False False
  mtu <- Clang.parse index Nothing args [] [Clang.TranslationUnit_None]
  when (isNothing mtu) $ error "No TXUnit!"
  let tu = fromJust mtu
  FFI.getInclusions tu printInclusions nullPtr
