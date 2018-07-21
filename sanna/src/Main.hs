{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

import Sanna.Prelude

import Development.Shake
import Development.Shake.FilePath (joinPath, splitDirectories, (</>))
import Development.Shake.Command (IsCmdArgument, CmdArguments)

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
    Miuspec -> miuspecMain p

getProject :: FilePath -> Project
getProject
  = splitDirectories
  .> break (== "miu")
  .> \(joinPath -> pre, post) ->
       let root = pre </> "miu" in
         case post of
           [] -> error "Couldn't find the miu directory on the cwd path.\
                       \Exiting :("
           "miu":ps -> case ps of
             [] -> Rooted { proj = Miu, root, full = root }
             "miuki":_ -> Rooted { proj = Miuki, root, full = root </> "miuki" }
             "miuspec":_ -> Rooted { proj = Miuspec, root, full = root </> "miuspec" }
             _ -> error "Not sure which project you're trying to build.\n\
                        \Maybe update the build system to handle it?"
           _ -> error "Unreachable!"

data ProjectName = Miu | Miuki | Miuspec
  deriving Eq

relativePath :: ProjectName -> FilePath
relativePath = \case
  Miu -> ""
  Miuki -> "miuki"
  Miuspec -> "miuspec"

data Project = Rooted {root :: FilePath, full :: FilePath, proj :: ProjectName}

changeProject :: Project -> ProjectName -> Project
changeProject Rooted{root} p
  = Rooted{root, full = root </> relativePath p, proj = p}

fwdOpts :: OptDescr (Either a String)
fwdOpts = Option "f" ["fwd"] (ReqArg (Right . id) "FLAGS")
  "Forward arguments to the underlying command."

type CmdRunner a b = (IsCmdArgument a, CmdArguments b) => a -> String -> b

fwdCmd :: [String] -> CmdRunner a b
fwdCmd flags a b = cmd a (b ++ ' ' : intercalate " " flags)

refreshRule :: Project -> CmdRunner [CmdOption] (Action ()) -> Rules ()
refreshRule Rooted{root} run =
  phony "refresh" <|
    run [Cwd (root </> "sanna")] "stack install"

miuMain :: Project -> IO ()
miuMain p = do
  assert (proj p == Miu) (pure ())
  miukiMain (p `changeProject` Miuki)

fire :: FilePath -> a -> Action ()
fire = const . need . (:[])

miuspecMain :: Project -> IO ()
miuspecMain p@Rooted{root, full, proj} = do
  assert (proj == Miuspec) (pure ())
  shakeArgsWith shakeOptions{shakeFiles = root </> ".sanna"} [fwdOpts]
    <| \flags targets -> return . Just <| do
      -- Don't forget the next line or Shake won't do anything!
      want targets
      let fwd_ = fwdCmd flags

      phony "clean" <|
        removeFilesAfter full ["lang.pdf"]

      refreshRule p fwd_

      phony "__generatePDF" <|
        fwd_ [Cwd full] "rst2pdf lang.rst -o lang.pdf -s kerning"

      full </> "lang.pdf" %> fire "__generatePDF"

      full </> "lang.rst" %> fire "__generatePDF"

      phony "build" <|
        need [full </> "lang.rst", full </> "lang.pdf"]

miukiMain :: Project -> IO ()
miukiMain p@Rooted{root, full, proj} = do
  assert (proj == Miuki) (pure ())
  shakeArgsWith shakeOptions{shakeFiles = root </> ".sanna"} [fwdOpts]
    <| \flags targets -> return . Just $ do
      action doSanityChecks
      let benchPath = full </> "bench"
      let sampleDir = benchPath </> "samples"
      let fwd_ = fwdCmd flags

      if null targets then action (fwd_ [Cwd full] "cargo") else want targets

      phony "clean" <|
        fwd_ [Cwd full] "cargo clean"

      refreshRule p fwd_

      phony "build" <|
        fwd_ [Cwd full] "cargo build"

      phony "__generateMiuFiles" <|
        fwd_ [Cwd sampleDir] "python3 generate.py"

      sampleDir </> "*.miu" %> fire "__generateMiuFiles"

      sampleDir </> "generate.py" %> fire "__generateMiuFiles"

      phony "bench" <| do
        -- if either thing changes, their rules should fire
        need [sampleDir </> "generate.py", sampleDir </> "10k.miu"]
        fwd_ [Cwd full] "cargo bench"

      phony "test" <|
        fwd_ [Cwd full] "cargo test"

doSanityChecks :: Action ()
doSanityChecks = getEnv "RUSTUP_TOOLCHAIN" >>= \case
  Nothing -> pure ()
  Just z  -> error
    (  "You've set the RUSTUP_TOOLCHAIN environmental variable to\n"
    ++ z
    ++ "\nwhich may mess up the build.\nPlease unset it before building."
    )
