{-# LANGUAGE OverloadedStrings #-}

module Main where


import Data.PublicSuffix
import Criterion.Main

import qualified Data.Text as T

suffixes :: [(String,T.Text)]
suffixes = [ ( "Simple TLD (org)",             "org"       )
           , ( "Simple ccTLD (ac)",            "ac"        )
           , ( "Simple gTLD (zurich)",         "zurich"    )
           , ( "Second level ccTLD (com.ve)",  "com.ve"    )
           , ( "Non-existent TLD (dontexist)", "dontexist" )
           ]

main :: IO ()
main = do
    defaultMain
        [ bgroup "publicSuffix" $ 
                 map (\(n,s) -> bench n $ nf publicSuffix s)
                     suffixes
        ]
