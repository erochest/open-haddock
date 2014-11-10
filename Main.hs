{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}


module Main where


import           Control.Monad
import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS hiding (FilePath)
import           Options.Applicative       hiding (command)
import qualified Options.Applicative       as A
import           Prelude                   hiding (FilePath)
import           Shelly

default (T.Text)


type PackageName = T.Text
data OpenHaddock = ListHs PackageName
                 | OpenHs PackageName
                 deriving (Show, Eq)


openHaddock :: OpenHaddock -> Sh ()

openHaddock (ListHs package) = do
    echo $ "searching for '" <> package <> "'"
    mapM_ echo =<< findPackageIndex package

openHaddock (OpenHs package) = undefined

findPackageIndex :: T.Text -> Sh [T.Text]
findPackageIndex package =
        filter (package `T.isInfixOf`)
    .   map toTextIgnore
    <$> findWhen (return . ("index.html" ==) . filename) ".cabal-sandbox"


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
    where
        nameArg = argument (T.pack <$> str)
                           (help "The name of the package to show haddocks for.")
        list    = ListHs <$> nameArg
        open    = OpenHs <$> nameArg

opt :: ParserInfo OpenHaddock
opt = info (helper <*> opt')
           (  fullDesc
           <> progDesc "Utilities for opening haddocks."
           <> header "open-haddock -- utilities for opening local haddocks.")
