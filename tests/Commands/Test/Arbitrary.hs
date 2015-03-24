{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs #-}
{-# LANGUAGE ScopedTypeVariables                                   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Commands.Test.Arbitrary where
import           Commands.Etc
import           Commands.Frontends.Dragon13.Text
import           Commands.Frontends.Dragon13.Types
import           Commands.Test.Etc
import           Commands.Test.Types
import           Control.Applicative
import           Data.List.NonEmpty                (NonEmpty (..))
import           Data.Text.Lazy                    (Text)
import qualified Data.Text.Lazy                    as T
import           Debug.Trace                       (traceShowId)
import           GHC.Exts                          (IsList (..))
import           Test.QuickCheck


instance (Arbitrary i, Arbitrary n, Arbitrary t) => Arbitrary (DNSGrammar i n t) where
 arbitrary = DNSGrammar <$> resized 2 arbitrary <*> resized 2 arbitrary <*> pure [] -- TODO  pure []

instance (Arbitrary i, Arbitrary n, Arbitrary t) => Arbitrary (DNSProduction i n t) where
 arbitrary = DNSProduction <$> arbitrary <*> arbitrary <*> resized 2 arbitrary

instance (Arbitrary i, Arbitrary n, Arbitrary t) => Arbitrary (DNSVocabulary i n t) where
 arbitrary = DNSVocabulary <$> arbitrary <*> arbitrary <*> resized 2 arbitrary

instance (Arbitrary n, Arbitrary t) => Arbitrary (DNSRHS n t) where
 arbitrary = oneof
  [ DNSTerminal     <$> arbitrary

  , (DNSNonTerminal . SomeDNSLHS) <$> (arbitrary :: Gen (DNSLHS LHSList n)) -- avoids @OverlappingInstances@
  , (DNSNonTerminal . SomeDNSLHS) <$> (arbitrary :: Gen (DNSLHS LHSRule n))

  , DNSOptional     <$> resized 2 arbitrary
  , DNSMultiple     <$> resized 2 arbitrary

  , DNSSequence     <$> resized 2 arbitrary
  , DNSAlternatives <$> resized 2 arbitrary
  ]

instance (Arbitrary n) => Arbitrary (DNSLHS LHSList n) where
 arbitrary = oneof
  [ DNSList        <$> arbitrary
  , DNSBuiltinList <$> arbitrary
  ]

instance (Arbitrary n) => Arbitrary (DNSLHS LHSRule n) where
 arbitrary = oneof
  [ DNSRule        <$> arbitrary
  , DNSBuiltinRule <$> arbitrary
  ]

instance (Arbitrary t) => Arbitrary (DNSToken t) where
 arbitrary = oneof
  [ DNSToken      <$> arbitrary
  , DNSPronounced <$> arbitrary <*> arbitrary
  ]

instance Arbitrary DNSBuiltinRule where arbitrary = elements constructors

instance Arbitrary DNSBuiltinList where arbitrary = elements constructors

instance Arbitrary Text where arbitrary = T.pack <$> arbitrary

instance Arbitrary a => Arbitrary (NonEmpty a) where arbitrary = (:|) <$> arbitrary <*> arbitrary

instance Arbitrary DNSText where
 arbitrary = do
  n <- choose (1,3)
  let s = T.pack <$> resize n arbitrary
  DNSText <$> (s `suchThat` isDNSText)

instance Arbitrary DNSName where
 arbitrary = do
  n <- choose (1,3)
  let s = T.pack <$> resize n arbitrary
  DNSName <$> (s `suchThat` isDNSName)
