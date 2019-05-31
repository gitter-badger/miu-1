#!/usr/bin/env stack
{- stack
   runghc
   --package text
   --package directory
   -- -O0
-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Directory as D
import Control.Monad (when, forM_)

make10k :: T.Text
make10k =  (<> "type alias Thing = Int\n") $ T.concat $ map f [1 .. 3333]
  where f i = let t = T.pack (show i) in
                  "type Dummy" <> t <> "\n"
               <> "  = Foo" <> t <> " Thing\n"
               <> "  | Bar" <> t <> " Dummy\n"

files :: [(FilePath, T.Text)]
files = [("10k.miu", make10k)]

readOnly :: D.Permissions
readOnly = D.emptyPermissions{D.readable = True, D.writable = False, D.searchable = True}

main :: IO ()
main = forM_ files $ \(p, s) -> do
  present <- D.doesFileExist p
  when present (D.removeFile p)
  T.writeFile p s
  D.setPermissions p readOnly
