{-# LANGUAGE OverloadedStrings #-}


module Main where


import qualified Data.Text           as T
import           Options.Applicative


type PackageName = T.Text
data OpenHaddock = ListHs PackageName
                 | OpenHs PackageName
                 deriving (Show, Eq)


main :: IO ()
main = print =<< execParser opt


opt' :: Parser OpenHaddock
opt' = subparser $  command "list" (info (helper <*> list)
                                          $  briefDesc
                                          <> progDesc "List haddocks for a package."
                                          <> header "open-haddock list -- list haddocks for a package.")
                 <> command "open" (info (helper <*> open)
                                         $  briefDesc
                                         <> progDesc "Open haddocks for a package."
                                         <> header "open-haddock open -- open haddocks for a package.")
    where
        list :: Parser OpenHaddock
        list = ListHs <$> nameArg

        open :: Parser OpenHaddock
        open = OpenHs <$> nameArg

        nameArg = option (T.pack <$> str)
                         (help "The name of the package to show haddocks for.")

opt :: ParserInfo OpenHaddock
opt = info (helper <*> opt')
           (  fullDesc
           <> progDesc "Utilities for opening haddocks."
           <> header "open-haddock -- utilities for opening local haddocks.")
