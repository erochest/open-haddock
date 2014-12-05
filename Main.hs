{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}


module Main where


import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Maybe
import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS hiding (FilePath, (</>))
import           Options.Applicative       hiding (command)
import qualified Options.Applicative       as A
import           Prelude                   hiding (FilePath)
import           Shelly

default (T.Text)


type PackageName = T.Text
type ModuleName  = T.Text
data OpenHaddock = ListHs FilePath PackageName
                 | OpenHs FilePath PackageName
                 | OpenModule FilePath ModuleName
                 deriving (Show, Eq)


openHaddock :: OpenHaddock -> Sh ()

openHaddock (ListHs cabal package) = do
    echo $ "searching for haddocks for '" <> package <> "'"
    void $ hcPkg cabal "list" [package]

openHaddock (OpenHs cabal package) = do
    echo $ "opening haddocks for '" <> package <> "'"
    maybe (return ()) open_ =<< findPackageIndex cabal package

openHaddock (OpenModule cabal modName) = do
    echo $ "opening haddocks for the package containing '" <> modName <> "'"
    maybe (return ()) open_ =<< findModuleIndex cabal modName

findPackageIndex :: FilePath -> PackageName -> Sh (Maybe FilePath)
findPackageIndex cabal package = runMaybeT $
    whenExists =<< MaybeT (   parseHaddockHtml
                          <$> hcPkg cabal "field" [package, "haddock-html"])

findModuleIndex :: FilePath -> ModuleName -> Sh (Maybe FilePath)
findModuleIndex cabal modName =
        maybe (return Nothing) (findPackageIndex cabal)
    =<< listToMaybe
    .   filter (/= "(no packages)")
    .   map T.strip
    .   filter (T.isPrefixOf "    ")
    .   T.lines
    <$> hcPkg cabal "find-module" [modName]


open_ :: FilePath -> Sh ()
open_ = command_ "open" [] . pure . toTextIgnore

hcPkg :: FilePath -> T.Text -> [T.Text] -> Sh T.Text
hcPkg cabal c = command1 cabal ["hc-pkg"] "sandbox" . (c :)

parseHaddockHtml :: T.Text -> Maybe FilePath
parseHaddockHtml =
      fmap ((</> "index.html") . fromText . T.strip . snd . T.breakOn " ")
    . listToMaybe
    . T.lines

whenExists :: FilePath -> MaybeT Sh FilePath
whenExists fn = do
    exists <- lift $ test_f fn
    if exists
        then return fn
        else fail $ "File not found: " ++ encodeString fn


main :: IO ()
main = shelly . verbosely . openHaddock =<< execParser opt


opt' :: Parser OpenHaddock
opt' = subparser $  A.command "list" (info (helper <*> list)
                                           $  briefDesc
                                           <> progDesc "List haddocks for a package."
                                           <> header "open-haddock list -- list haddocks for a package.")
                 <> A.command "open" (info (helper <*> open)
                                           $  briefDesc
                                           <> progDesc "Open haddocks for a package."
                                           <> header "open-haddock open -- open haddocks for a package.")
                 <> A.command "module" (info (helper <*> mod_)
                                             $  briefDesc
                                             <> progDesc "Open haddocks for a module's package."
                                             <> header "open-haddock module -- open haddocks for a\
                                                       \ module's package.")
    where
        nameArg = argument (T.pack <$> str)
                           (help "The name of the package to show haddocks for.")
        cabalOpt = option (decodeString <$> str)
                          (  short 'c' <> long "cabal" <> metavar "CABAL"
                          <> value "cabal"
                          <> help "The cabal executable name.\
                                  \ Defaults to 'cabal'.")
        list    = ListHs     <$> cabalOpt <*> nameArg
        open    = OpenHs     <$> cabalOpt <*> nameArg
        mod_    = OpenModule <$> cabalOpt <*> nameArg

opt :: ParserInfo OpenHaddock
opt = info (helper <*> opt')
           (  fullDesc
           <> progDesc "Utilities for opening haddocks."
           <> header "open-haddock -- utilities for opening local haddocks.")
