import System.Environment(getArgs)
import Control.Monad.IO.Class(liftIO)
import Text.Printf(printf)
import Clang.String(unpack)
import Clang.TranslationUnit(withCreateIndex, withParse, TranslationUnitFlags(..), getCursor)
import Clang.File(getName)
import Clang.Traversal(getChildren)
import Clang.Cursor(getKind, getDisplayName, getCursorKindSpelling)
import qualified Data.Vector as DV(empty)
import qualified Data.Vector.Storable as DVS(mapM_)

test tu = getCursor tu >>= getChildren >>= DVS.mapM_ printInfo
    where printInfo c = do
            name <- getDisplayName c >>= unpack
            tstr <- getCursorKindSpelling (getKind c) >>= unpack
            liftIO $ printf "Name:%s, Kind:%s\n" name tstr

main = do
  (arg:args) <- getArgs
  withCreateIndex False False $ \index -> 
      withParse index (Just arg) args DV.empty [TranslationUnit_None] test (error "No TXUnit!")
