{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

import Sanna.Prelude

import Development.Shake
import Development.Shake.FilePath (joinPath, splitDirectories, (</>))
import Development.Shake.Command (IsCmdArgument, CmdArguments)

import Text.Read (readMaybe)
import System.Console.GetOpt (ArgDescr (ReqArg), OptDescr (Option))
import System.Directory (getCurrentDirectory)

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let p = getProject cwd
  mainFor (name p) p

getProject :: FilePath -> Project
getProject =
     splitDirectories
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
      []  -> Rooted {root, full = root, name = Miu, primaryProject = Miu}
      s:_
        | Just name <- readMaybe s
          -> Rooted {root, full = root </> s, name, primaryProject = name}
      ps  -> error <|
        "Not sure which project you're trying to build.\n\
        \Maybe update the build system to handle it?\n\
        \Here's the path I found\n\n\t" ++ show ps

data ProjectName = Miu | Miudoc | Miuki | MiukiHs | Miuspec | Numark | Sanna | Sojiro
  deriving (Bounded, Enum, Eq)

allProjects :: [ProjectName]
allProjects = [Miu .. Sojiro]

instance Read ProjectName where
  readsPrec _ s = case s of
    'm':'i':'u':'k':'i':'-':'h':'s':rest -> [(MiukiHs, rest)]
    'm':'i':'u':'k':'i'            :rest -> [(Miuki  , rest)]
    'm':'i':'u':'s':'p':'e':'c'    :rest -> [(Miuspec, rest)]
    'm':'i':'u':'d':'o':'c'        :rest -> [(Miudoc , rest)]
    'm':'i':'u'                    :rest -> [(Miu    , rest)]
    'n':'u':'m':'a':'r':'k'        :rest -> [(Numark , rest)]
    's':'a':'n':'n':'a'            :rest -> [(Sanna  , rest)]
    's':'o':'j':'i':'r':'o'        :rest -> [(Sojiro , rest)]
    _ -> []

relativePath :: ProjectName -> FilePath
relativePath = \case
  Miu     -> ""
  Miudoc  -> "miudoc"
  Miuki   -> "miuki"
  MiukiHs -> "miuki-hs"
  Miuspec -> "miuspec"
  Numark  -> "numark"
  Sanna   -> "sanna"
  Sojiro  -> "sojiro"

data Project = Rooted
  { root :: FilePath
  , full :: FilePath
  , name :: ProjectName
  , primaryProject :: ProjectName
  }

changeProject :: Project -> ProjectName -> Project
changeProject Rooted{root, primaryProject} p =
  Rooted{root, full = root </> relativePath p, name = p, primaryProject}

newtype Flag = Flag String
  deriving Eq

newtype ShellCmd = ShellCmd String

instance IsString ShellCmd where
  fromString = coerce

newtype Target = Target String
  deriving Show

instance IsString Target where
  fromString = coerce

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
-- The "prebuilt" argument avoids punting the work onto Cargo's build.rs file,
-- as we are better positioned to work with it here.
cargoBoilerplate :: Project -> [Flag] -> [Target] -> [FilePath] -> Rules ACmdRunner
cargoBoilerplate p@Rooted{full} flags targets prebuilt = do
    action runRustSanityChecks
    let fwd_ = fwdCmd flags
    if null targets then fwdToCargo fwd_ else want (coerce targets)
    refreshRule p fwd_
    basicRules fwd_
    pure fwd_
  where
    fwdToCargo f = action (f [Cwd full] "cargo")
    basicRules run = do
      phony "build" <| do
        unless (null prebuilt) <| need ((full </> "build.rs") : prebuilt)
        run [Cwd full] "cargo build --color=always"
      phony "test"  <| run [Cwd full] "cargo test"
      phony "clean" <| run [Cwd full] "cargo clean"
{-# ANN cargoBoilerplate ("HLint: ignore Reduce duplication" :: String) #-}
-- We can refactor common stuff later if needed.

stackBoilerplate :: Project -> [Flag] -> [Target] -> Rules ACmdRunner
stackBoilerplate p@Rooted{full} flags targets = do
    let fwd_ = fwdCmd flags
    if null targets then fwdToStack fwd_ else want (coerce targets)
    refreshRule p fwd_
    basicRules fwd_
    pure fwd_
  where
    fwdToStack f = action (f [Cwd full] "stack")
    basicRules run = do
      phony "build" <| run [Cwd full] "stack build"
      phony "test"  <| run [Cwd full] "stack test"
      phony "clean" <| run [Cwd full] "stack clean"

miuShakeArgs
  :: FilePath
  -> ([Flag] -> [Target] -> IO (Maybe (Rules ())))
  -> IO ()
miuShakeArgs root run =
  shakeArgsWith shakeOptions{shakeFiles = root </> ".sanna"} [fwdOpts] run'
  where
    run' fs ts = run fs (map fromString ts)

--------------------------------------------------------------------------------
-- * Main functions

defaultMain :: RuleBuilder -> ProjectName -> Project -> IO ()
defaultMain f pname p =
  assert (name p == pname)
    miuShakeArgs (root p) <| \fs ts -> pure (Just (f p fs ts))

miuMain :: Project -> IO ()
miuMain p =
  forM_ allProjects <| \pj ->
    defaultMain (rulesFor pj) pj (p `changeProject` pj)

mainFor :: ProjectName -> Project -> IO ()
mainFor = \case
  Miu -> miuMain
  n   -> \p -> defaultMain (rulesFor n) n (p `changeProject` n)

rulesFor :: ProjectName -> RuleBuilder
rulesFor = \case
  Miu     -> miuRules
  Miudoc  -> miudocRules
  Miuki   -> miukiRules
  MiukiHs -> miukiHsRules
  Miuspec -> miuspecRules
  Numark  -> numarkRules
  Sanna   -> sannaRules
  Sojiro  -> sojiroRules

--------------------------------------------------------------------------------
-- * Rules

type ACmdRunner = [CmdOption] -> ShellCmd -> Action ()

fwdOpts :: OptDescr (Either a Flag)
fwdOpts = Option "f" ["fwd"] (ReqArg (Right . Flag) "FLAGS")
  "Forward arguments to the underlying command."

refreshRule :: Project -> ACmdRunner -> Rules ()
refreshRule Rooted{root, name, primaryProject} run =
  phony "refresh" <|
    when (name == primaryProject) <|
      run [Cwd (root </> "sanna")] "stack install"

type RuleBuilder = Project -> [Flag] -> [Target] -> Rules ()

defaultHaskellRules, defaultRustRules :: RuleBuilder
miuRules, miudocRules, miukiRules, miukiHsRules, miuspecRules :: RuleBuilder
numarkRules, sannaRules, sojiroRules :: RuleBuilder

defaultHaskellRules a b c = void (stackBoilerplate a b c)
defaultRustRules    a b c = void (cargoBoilerplate a b c [])

miuRules p flags targets = do
  let fwd_ = fwdCmd flags
  want (filter (== "refresh") <| coerce targets)
  refreshRule p fwd_

miudocRules = defaultRustRules

miukiTsParserRules :: FilePath -> Rules [FilePath]
miukiTsParserRules full = do
  let parserDir = full </> "miu-ts-parser"
  let treeSitterBinary =
        joinPath [parserDir, "node_modules", "tree-sitter-cli", "tree-sitter"]

  treeSitterBinary %> \_ ->
    cmd_ [Cwd parserDir] ("npm install" :: String)

  joinPath [parserDir, "src", "parser.c"] %> \_ -> do
    need [treeSitterBinary]
    cmd_ (Cwd parserDir) (treeSitterBinary ++ " generate")

  joinPath [parserDir, "src", "parser.o"] %> \_ -> do
    need [parserDir </> "src" </> "parser.c"]
    cmd_ [Cwd (parserDir </> "src")] ("clang -fPIC -I . -c parser.c" :: String)

  joinPath [parserDir, "libmiuparser.a"] %> \_ -> do
    need [parserDir </> "src" </> "parser.o"]
    cmd_ [Cwd parserDir] ("ar crs libmiuparser.a " ++ ("src" </> "parser.o"))

  pure [parserDir </> "libmiuparser.a"]

miukiRules p@Rooted{full} flags targets = do

  prebuilt <- miukiTsParserRules full

  fwd_ <- cargoBoilerplate p flags targets prebuilt
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

miukiHsRules = defaultHaskellRules

miuspecRules p@Rooted{full} flags targets = do
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

numarkRules = defaultRustRules
sannaRules  = defaultHaskellRules
sojiroRules = defaultRustRules

--------------------------------------------------------------------------------
-- * Helper functions

fire :: FilePath -> a -> Action ()
fire p = const (need [p])

fwdCmd
  :: ( Coercible flags [String], IsCmdArgument args
     , Coercible shcmd String, CmdArguments ret)
  => flags -> args -> shcmd -> ret
fwdCmd flags args shcmd =
  cmd args (coerce shcmd ++ ' ' : unwords (coerce flags))
