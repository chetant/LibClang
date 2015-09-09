import System.Environment(getArgs)
import Control.Monad.IO.Class(liftIO)
import qualified Data.Vector.Storable as DVS(mapM_)

import Clang.String(unpack)
import Clang.File(getName)
import Clang(parseSourceFile, getInclusions, Inclusion(..))

test tu = getInclusions tu >>= DVS.mapM_ printInclusion
    where printInclusion (Inclusion fname _ _) = do
            name <- getName fname >>= unpack
            liftIO $ putStrLn $ "Included:" ++ name

main = do
  (arg:args) <- getArgs
  parseSourceFile arg args test
