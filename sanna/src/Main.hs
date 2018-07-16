{-# LANGUAGE ViewPatterns #-}

import Sanna.Prelude

import Development.Shake
import Development.Shake.FilePath (joinPath, splitDirectories, (</>))

import Control.Exception (assert)
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

data Project = Rooted { root :: FilePath, full :: FilePath, proj :: ProjectName }

changeProject :: Project -> ProjectName -> Project
changeProject Rooted{root} p = Rooted{root, full = root </> relativePath p, proj = p}

miuMain :: Project -> IO ()
miuMain p = do
  assert (proj p == Miu) (pure ())
  miukiMain (p `changeProject` Miuki)

-- TODO: Add forwarding of arguments to cargo
-- https://github.com/ndmitchell/shake/issues/607
miukiMain :: Project -> IO ()
miukiMain Rooted{root, full, proj} = do
  assert (proj == Miuki) (pure ())
  shakeArgs shakeOptions{ shakeFiles = full </> ".sanna" } $ do
    action doSanityChecks

    let benchPath = full </> "bench"
    let sampleDir = benchPath </> "samples"

    phony "generateMiuFiles" $
      cmd [Cwd sampleDir] "python3 generate.py"

    sampleDir </> "*.miu" %> \_ ->
      need ["generateMiuFiles"]

    sampleDir </> "generate.py" %> \_ ->
      need ["generateMiuFiles"]

    phony "clean" $
      cmd [Cwd full] "cargo clean"

    phony "refresh" $
      cmd [Cwd (root </> "sanna")] "stack install"

    phony "build" $
      cmd [Cwd full] "cargo build"

    phony "bench" $ do
      -- if either thing changes, their rules should fire
      need [sampleDir </> "generate.py", sampleDir </> "10k.miu"]
      cmd [Cwd full] "cargo bench"

doSanityChecks :: Action ()
doSanityChecks = getEnv "RUSTUP_TOOLCHAIN" >>= \case
  Nothing -> pure ()
  Just z  -> error
    (  "You've set the RUSTUP_TOOLCHAIN environmental variable to\n"
    ++ z
    ++ "\nwhich may mess up the build.\nPlease unset it before building."
    )
