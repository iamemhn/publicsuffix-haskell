module Data.PublicSuffix.Types where

import Data.Text ( Text ) 

data Rule = Rule
    { isException :: !Bool
      -- ^ Whether this rule is an exception or not.

    , ruleLabels  :: ![Text]
      -- ^ The domain labels in reversed order, ie:
      --
      --   > "test.example.com" => ["com", "example", "test"]

    } deriving (Show)
