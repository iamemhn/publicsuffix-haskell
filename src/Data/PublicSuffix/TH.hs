{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.PublicSuffix.TH
    ( moduleDirectory
    , mkRules
    ) where


import           Control.Applicative

import           Data.Char
import           Data.PublicSuffix.Types

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import           Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax as TH

import           System.FilePath            (dropFileName)

import           Prelude



readRulesFile :: FilePath -> IO [Rule]
readRulesFile inputFile = do
    body <- TIO.readFile inputFile
    return $ parseRules body

isComment :: T.Text -> Bool
isComment = T.isPrefixOf "//"

splitDot :: T.Text -> [T.Text]
splitDot ts = if T.null ts then [""]
                           else T.splitOn "." ts

parseRules :: T.Text -> [Rule]
parseRules body =
    map parseRule $
    filter ruleLine $                   -- Only keep rules
    map (T.takeWhile (not . isSpace)) $ -- read up to the first whitespace.
    T.lines body                        -- Break list in lines.

  where
    ruleLine :: T.Text -> Bool
    ruleLine line = not $ isComment line || T.null line

    parseRule :: T.Text -> Rule
    parseRule line
      | T.null line            = error "parseRule: unexpected empty line"
      | T.isPrefixOf "!"  line = Rule { isException = True,
                                        ruleLabels  = splitDot $ T.tail line
                                      }
      | otherwise              = Rule { isException = False,
                                        ruleLabels  = splitDot line 
                                      }


moduleDirectory :: Q Exp
moduleDirectory =
    TH.LitE . TH.StringL . dropFileName . TH.loc_filename <$> TH.qLocation


mkRules :: T.Text -> FilePath -> Q [Dec]
mkRules funName filePath = do
    rules <- runIO $ readRulesFile filePath
    rulesE <- mapM genRule rules

    return
        [ SigD (mkName "rules") (AppT ListT (ConT ''Rule))
        , FunD (mkName (T.unpack funName)) [Clause [] (NormalB $ ListE $ rulesE) []]
        ]

  where
    genRule :: Rule -> ExpQ
    genRule rule = do
        ruleE     <- [| Rule |]
        trueE     <- [| True |]
        falseE    <- [| False |]

        return $ foldl1 AppE
            [ ruleE
            , if isException rule then trueE else falseE
            , ListE $ reverse $ map (LitE . StringL . T.unpack)
                                    (ruleLabels rule)
            ]
