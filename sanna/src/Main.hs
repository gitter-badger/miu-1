{-# LANGUAGE ViewPatterns #-}

import Sanna.Prelude

import Development.Shake
import Development.Shake.FilePath (joinPath, splitDirectories, (</>))

import Control.Exception (assert)
import Data.List (intercalate)
import System.Console.GetOpt (ArgDescr (ReqArg), OptDescr (Option))
import System.Directory (getCurrentDirectory)

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let p = getProject cwd
  case proj p of
    Miu   -> miuMain p
    Miuki -> miukiMain p

getProject :: FilePath -> Project
getProject
  = splitDirectories
  .> break (== "miu")
  .> \(joinPath -> pre, post) ->
       let root = pre </> "miu" in
         case drop 1 post of
           [] ->        Rooted { proj = Miu, root, full = root }
           "miuki":_ -> Rooted { proj = Miuki, root, full = root </> "miuki" }
           _ -> error "Neither miu nor miuki were found on the current path.\n\
                      \Exiting. :("

data ProjectName = Miu | Miuki
  deriving Eq

relativePath :: ProjectName -> FilePath
relativePath = \case
  Miu -> ""
  Miuki -> "miuki"

data Project = Rooted {root :: FilePath, full :: FilePath, proj :: ProjectName}

changeProject :: Project -> ProjectName -> Project
changeProject Rooted{root} p
  = Rooted{root, full = root </> relativePath p, proj = p}

miuMain :: Project -> IO ()
miuMain p = do
  assert (proj p == Miu) (pure ())
  miukiMain (p `changeProject` Miuki)

fwdOpts :: OptDescr (Either a String)
fwdOpts = Option "f" ["fwd"] (ReqArg (Right . id) "FLAGS")
  "Forward arguments to the underlying command."

miukiMain :: Project -> IO ()
miukiMain Rooted{root, full, proj} = do
  assert (proj == Miuki) (pure ())
  shakeArgsWith shakeOptions{shakeFiles = full </> ".sanna"} [fwdOpts]
    $ \flags targets -> return . Just $ do
      action doSanityChecks
      let benchPath = full </> "bench"
      let sampleDir = benchPath </> "samples"
      let fwdCmd a b = cmd a (b ++ ' ' : intercalate " " flags)

      if null targets then action (fwdCmd [Cwd full] "cargo") else want targets

      phony "__generateMiuFiles" $
        fwdCmd [Cwd sampleDir] "python3 generate.py"

      sampleDir </> "*.miu" %> \_ ->
        need ["__generateMiuFiles"]

      sampleDir </> "generate.py" %> \_ ->
        need ["__generateMiuFiles"]

      phony "clean" $
        fwdCmd [Cwd full] "cargo clean"

      phony "refresh" $
        fwdCmd [Cwd (root </> "sanna")] "stack install"

      phony "build" $
        fwdCmd [Cwd full] "cargo build"

      phony "bench" $ do
        -- if either thing changes, their rules should fire
        need [sampleDir </> "generate.py", sampleDir </> "10k.miu"]
        fwdCmd [Cwd full] "cargo bench"

      phony "test" $
        fwdCmd [Cwd full] "cargo test"

doSanityChecks :: Action ()
doSanityChecks = getEnv "RUSTUP_TOOLCHAIN" >>= \case
  Nothing -> pure ()
  Just z  -> error
    (  "You've set the RUSTUP_TOOLCHAIN environmental variable to\n"
    ++ z
    ++ "\nwhich may mess up the build.\nPlease unset it before building."
    )
