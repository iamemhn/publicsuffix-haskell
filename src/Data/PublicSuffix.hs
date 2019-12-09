{-# LANGUAGE OverloadedStrings #-}

module Data.PublicSuffix
    ( publicSuffix
    , registeredDomain
    ) where

import           Data.Function
import           Data.List

import qualified Data.Text as T

import           Data.PublicSuffix.Types
import           Data.PublicSuffix.Rules

import           Prelude


-- | Convert a domain into a list of labels.
-- This is essentialy splitting the string on dots ('.')
toLabels :: T.Text -> [T.Text]
toLabels = T.splitOn "."

-- | A domain is said to match a rule if and only if all of the following
-- conditions are met:
--
--  - When the domain and rule are split into corresponding labels, that the
--    domain contains as many or more labels than the rule.
--
--  - Beginning with the right-most labels of both the domain and the rule,
--    and continuing for all labels in the rule, one finds that for every pair,
--    either they are identical, or that the label from the rule is "*".

matchRule :: [T.Text] -> Rule -> Bool
matchRule domainLabels rule =
    domainLabelsLength >= ruleLabelsLength &&
    all labelMatches (zip (ruleLabels rule) domainLabels)

  where
    ruleLabelsLength   = length $ ruleLabels rule
    domainLabelsLength = length domainLabels


-- | True if the label from the rule matches a label from the domain.
labelMatches :: (T.Text, T.Text) -> Bool
labelMatches ("*"      , _          ) = True
labelMatches (ruleLabel, domainLabel) = ruleLabel == domainLabel



-- | Return the public suffix of the given domain name (a dot-delimited unicode
-- string). The public suffix is the part of a domain which should be protected.
--
-- Notes:
--
--  - The domain MUST NOT start with a dot. Normalize the domain before passing
--    it to functions in this module.
--  - The domain MUST NOT be in punycode encoding. The matching of domain labels
--    is done on the original encoding, as specified in the upstream
--    publicsuffix list.
--
--
-- In particular that means applications SHOULD reject:
--
--  - HTTP cookies which try to set domain to a public suffix.
--  - X509 wildcard certificates which try to match all subdomains of a public
--    suffix.

publicSuffix :: T.Text -> T.Text
publicSuffix domain =
    -- Algorithm (see https://publicsuffix.org/list/)
    --
    --   Match domain against all rules and take note of the matching ones.
    --   If no rules match, the prevailing rule is "*".
    --   If more than one rule matches, the prevailing rule is the one which is an exception rule.
    --   If there is no matching exception rule, the prevailing rule is the one with the most labels.
    --   If the prevailing rule is a exception rule, modify it by removing the leftmost label.
    --   The public suffix is the set of labels from the domain which match the labels of the prevailing rule, using the matching algorithm above.
    --   The registered or registrable domain is the public suffix plus one additional label.

    T.intercalate "." $
    reverse $
    take numMatchingLabels domainLabels

  where
    rule              = prevailingRule domainLabels
    domainLabels      = reverse $ toLabels domain
    numMatchingLabels = length $ takeWhile labelMatches $ zip (ruleLabels rule) domainLabels


prevailingRule :: [T.Text] -> Rule
prevailingRule domainLabels = case filter (matchRule domainLabels) rules of
    []  -> Rule False ["*"]
    [x] -> x
    xs  -> case filter isException xs of
        []   -> head $ reverse $ sortBy (compare `on` (length . ruleLabels)) xs
        ex:_ -> Rule (isException ex) (init $ ruleLabels ex)



-- | Return the domain that was registered or is registrable by a user. These
-- domains are fully controlled by users, and applications SHOULD accept
-- cookies and wildcard certificates for those.

registeredDomain :: T.Text -> Maybe T.Text
registeredDomain domain = if domain == suffix
    then Nothing
    else Just $ T.intercalate "." $ reverse $ take (suffixLabelsLength + 1) domainLabels

  where
    suffix             = publicSuffix domain
    suffixLabelsLength = length $ toLabels suffix
    domainLabels       = reverse $ toLabels domain
