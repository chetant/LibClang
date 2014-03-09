import System.Environment(getArgs)
import Control.Monad.IO.Class(liftIO)
import Clang.String(unpack)
import Clang.TranslationUnit(withCreateIndex, withParse, TranslationUnitFlags(..))
import Clang.File(getName)
import Clang.Traversal(getInclusions, Inclusion(..))
import qualified Data.Vector as DV(empty)
import qualified Data.Vector.Storable as DVS(mapM_)

test tu = getInclusions tu >>= DVS.mapM_ printInclusion
    where printInclusion (Inclusion fname _ _) = do
            name <- getName fname >>= unpack
            liftIO $ putStrLn $ "Included:" ++ name

main = do
  (arg:args) <- getArgs
  withCreateIndex False False $ \index -> 
      withParse index (Just arg) args DV.empty [TranslationUnit_None] test (error "No TXUnit!")
