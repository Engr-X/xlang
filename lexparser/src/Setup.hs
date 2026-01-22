import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Verbosity

import System.Directory (doesFileExist)
import System.FilePath ((<.>), dropExtension)
import System.Process (callProcess)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \args flags -> do
      preprocessHappySources
      preBuild simpleUserHooks args flags
  }


-- English comment: Preprocess *.ypp -> *.y using cpp so Happy can read them.
preprocessHappySources :: IO ()
preprocessHappySources = do
  let ypps =
        [ "src/Parse/ParseExpr.ypp" ]
  mapM_ cppOne ypps


cppOne :: FilePath -> IO ()
cppOne ypp = do
  ok <- doesFileExist ypp
  if not ok
    then pure ()
    else do
      let outY = dropExtension ypp <.> "y"
      -- -P: no line markers, keeps Happy happier
      -- -I src: allow #include "Parse/GrammarExpr.inc"
      callProcess "cpp" ["-P", "-I", "src", ypp, outY]
