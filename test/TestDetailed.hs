{-# LANGUAGE PackageImports, RecordWildCards, NamedFieldPuns #-}
module TestDetailed (tests) where

import qualified Test.QuickCheck as Q
import Distribution.TestSuite as TS

import TestTrieAble
import TrieAble.Instances (propListOfInt32TrieAble, propListOfInt16TrieAble, propListOfInt64TrieAble)

toTSResult :: Q.Result -> TS.Result
toTSResult Q.Success {} = TS.Pass
toTSResult Q.GaveUp {} = TS.Fail "GaveUp"
toTSResult Q.Failure {reason} = TS.Fail reason


runQuickCheck :: Q.Testable p => p -> IO TS.Progress
runQuickCheck prop = do
        qres <- Q.quickCheckWithResult Q.stdArgs {Q.maxSuccess = 40, Q.maxSize = 40} prop
        return $ (Finished . toTSResult) qres
        
tests :: IO [Test]
tests = return [ Test $ TestInstance (runQuickCheck propSorted) "propSorted" ["text"] [] undefined,
                 Test $ TestInstance (runQuickCheck propListOfInt16TrieAble) "propListOfInt16TrieAble" ["list of ints"] [] undefined,        
                 Test $ TestInstance (runQuickCheck propListOfInt32TrieAble) "propListOfInt32TrieAble" ["list of ints"] [] undefined,
                 Test $ TestInstance (runQuickCheck propListOfInt64TrieAble) "propListOfInt64TrieAble" ["list of ints"] [] undefined
                 ]