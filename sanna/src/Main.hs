{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

import Sanna.Prelude

import Development.Shake
import Development.Shake.FilePath (joinPath, splitDirectories, (</>))
import Development.Shake.Command (IsCmdArgument, CmdArguments)

import Data.List (intercalate)
import System.Console.GetOpt (ArgDescr (ReqArg), OptDescr (Option))
import System.Directory (getCurrentDirectory)

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let p = getProject cwd
  case proj p of
    Miu     -> miuMain p
    Miudoc  -> miudocMain p
    Miuki   -> miukiMain p
    Miuspec -> miuspecMain p
    Sanna   -> sannaMain p
    Sojiro  -> sojiroMain p

getProject :: FilePath -> Project
getProject
  =  splitDirectories
  .> break (== "miu")
  .> \(joinPath -> pre, post) ->
       let root = pre </> "miu" in
         case post of
           [] -> error "Couldn't find the miu directory on the cwd path.\
                       \Exiting :("
           "miu":ps -> identifyLast root ps
           _ -> unreachable
  where
    identifyLast root = \case
      []          -> Rooted { root, full = root              , proj = Miu     }
      "miudoc" :_ -> Rooted { root, full = root </> "miudoc" , proj = Miudoc  }
      "miuki"  :_ -> Rooted { root, full = root </> "miuki"  , proj = Miuki   }
      "miuspec":_ -> Rooted { root, full = root </> "miuspec", proj = Miuspec }
      "sanna"  :_ -> Rooted { root, full = root </> "sanna"  , proj = Sanna   }
      "sojiro" :_ -> Rooted { root, full = root </> "sojiro" , proj = Sojiro  }
      _ -> error "Not sure which project you're trying to build.\n\
                 \Maybe update the build system to handle it?"

data ProjectName = Miu | Miudoc | Miuki | Miuspec | Sanna | Sojiro
  deriving Eq

relativePath :: ProjectName -> FilePath
relativePath = \case
  Miu     -> ""
  Miudoc  -> "miudoc"
  Miuki   -> "miuki"
  Miuspec -> "miuspec"
  Sanna   -> "sanna"
  Sojiro  -> "sojiro"

data Project = Rooted
  { root :: FilePath
  , full :: FilePath
  , proj :: ProjectName
  }

changeProject :: Project -> ProjectName -> Project
changeProject Rooted{root} p
  = Rooted{root, full = root </> relativePath p, proj = p}

newtype Flag = Flag String

fwdOpts :: OptDescr (Either a Flag)
fwdOpts = Option "f" ["fwd"] (ReqArg (Right . Flag) "FLAGS")
  "Forward arguments to the underlying command."

newtype ShellCmd = ShellCmd String

instance IsString ShellCmd where
  fromString = coerce

newtype Target = Target String
  deriving Show

instance IsString Target where
  fromString = coerce

type ACmdRunner a = Coercible a String => [CmdOption] -> a -> Action ()

refreshRule :: Project -> (ACmdRunner ShellCmd) -> Rules ()
refreshRule Rooted{root} run =
  phony "refresh" <|
    run [Cwd (root </> "sanna")] "stack install"

fire :: FilePath -> a -> Action ()
fire = (:[]) .> need .> const

fwdCmd
  :: (Coercible flags [String], IsCmdArgument args, Coercible shcmd String, CmdArguments ret)
  => flags -> args -> shcmd -> ret
fwdCmd flags = \args shcmd ->
  cmd args (coerce shcmd ++ ' ' : intercalate " " (coerce flags))

runRustSanityChecks :: Action ()
runRustSanityChecks = getEnv "RUSTUP_TOOLCHAIN" >>= \case
  Nothing -> pure ()
  Just z  -> error
    (  "You've set the RUSTUP_TOOLCHAIN environmental variable to\n"
    <> z
    <> "\nwhich may mess up the build.\nPlease unset it before building."
    )

-- | Returns a command runner function that forwards the extra flags to ShellCmd.
--
-- TODO: Replacing the return type with Rules (ACmdRunner ShellCmd) gives an
-- impredicative polymorphism related error. Investigate this...
cargoBoilerplate
  :: Project
  -> [Flag]
  -> [Target]
  -> Rules ([CmdOption] -> ShellCmd -> Action ())
cargoBoilerplate p@Rooted{full} flags targets = do
    action runRustSanityChecks
    let fwd_ = fwdCmd flags
    if null targets then fwdToCargo fwd_ else want (coerce targets)
    refreshRule p fwd_
    basicRules fwd_
    pure fwd_
  where
    fwdToCargo f = action (f [Cwd full] "cargo")
    basicRules run = do
      phony "build" <| run [Cwd full] "cargo build"
      phony "test"  <| run [Cwd full] "cargo test"
      phony "clean" <| run [Cwd full] "cargo clean"

stackBoilerplate
  :: Project
  -> [Flag]
  -> [Target]
  -> Rules ([CmdOption] -> ShellCmd -> Action ())
stackBoilerplate p@Rooted{full} flags targets = do
    let fwd_ = fwdCmd flags
    if null targets then fwdToCargo fwd_ else want (coerce targets)
    refreshRule p fwd_
    basicRules fwd_
    pure fwd_
  where
    fwdToCargo f = action (f [Cwd full] "stack")
    basicRules run = do
      phony "build" <| run [Cwd full] "stack build"
      phony "test"  <| run [Cwd full] "stack test"
      phony "clean" <| run [Cwd full] "stack clean"

miuShakeArgs
  :: FilePath
  -> ([Flag] -> [Target] -> IO (Maybe (Rules ())))
  -> IO ()
miuShakeArgs root run
  = shakeArgsWith shakeOptions{shakeFiles = root </> ".sanna"} [fwdOpts] run'
  where
    run' fs ts = run fs (map fromString ts)

miuMain :: Project -> IO ()
miuMain p = do
  assert (proj p == Miu) (pure ())
  -- Makes sense to update the build system first :)
  sannaMain   (p `changeProject` Sanna)
  -- Then the compiler
  miukiMain   (p `changeProject` Miuki)
  -- Stuff which depends on the compiler
  miudocMain  (p `changeProject` Miudoc)
  miuspecMain (p `changeProject` Miuspec)
  -- The server last as it will probably end up depending on everything else
  sojiroMain  (p `changeProject` Sojiro)

sannaMain :: Project -> IO ()
sannaMain p@Rooted{root, proj} = do
  assert (proj == Sanna) (pure ())
  miuShakeArgs root <| pure <. Just <. void <.: stackBoilerplate p

miudocMain :: Project -> IO ()
miudocMain p@Rooted{root, proj} = do
  assert (proj == Miudoc) (pure ())
  miuShakeArgs root <| pure <. Just <. void <.: cargoBoilerplate p

sojiroMain :: Project -> IO ()
sojiroMain p@Rooted{root, proj} = do
  assert (proj == Sojiro) (pure ())
  miuShakeArgs root <| pure <. Just <. void <.: cargoBoilerplate p

miukiMain :: Project -> IO ()
miukiMain p@Rooted{root, full, proj} = do
  assert (proj == Miuki) (pure ())
  miuShakeArgs root <| \flags targets -> pure <. Just <| do
      fwd_ <- cargoBoilerplate p flags targets

      let benchPath = full </> "bench"
          sampleDir = benchPath </> "samples"
      phony "__generateMiuFiles" <|
        fwd_ [Cwd sampleDir] "python3 generate.py"

      sampleDir </> "*.miu" %> fire "__generateMiuFiles"

      sampleDir </> "generate.py" %> fire "__generateMiuFiles"

      phony "bench" <| do
        -- if either thing changes, their rules should fire
        need [sampleDir </> "generate.py", sampleDir </> "10k.miu"]
        fwd_ [Cwd full] "cargo bench"

miuspecMain :: Project -> IO ()
miuspecMain p@Rooted{root, full, proj} = do
  assert (proj == Miuspec) (pure ())
  miuShakeArgs root <| \flags targets -> pure . Just <| do
      -- Don't forget the next line or Shake won't do anything!
      want (coerce targets)
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
