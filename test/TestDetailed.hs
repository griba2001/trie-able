{-# LANGUAGE PackageImports, RecordWildCards, NamedFieldPuns #-}
module TestDetailed (tests) where

import qualified Test.QuickCheck as Q
import Distribution.TestSuite as TS

import TestTrieAble
import TrieAble.Instances ()

toTSResult :: Q.Result -> TS.Result
toTSResult Q.Success {} = TS.Pass
toTSResult Q.GaveUp {} = TS.Fail "GaveUp"
toTSResult Q.Failure {reason} = TS.Fail reason


runQuickCheck :: Q.Testable p => p -> IO TS.Progress
runQuickCheck propTrieOfKey = do
        qres <- Q.quickCheckWithResult Q.stdArgs {Q.maxSuccess = 40, Q.maxSize = 40} propTrieOfKey
        return $ (Finished . toTSResult) qres
        
tests :: IO [Test]
tests = return [ Test $ TestInstance (runQuickCheck propTrieOfKeyTextSorted) "propTrieOfKeyTextSorted" ["text"] [] undefined,
                 Test $ TestInstance (runQuickCheck propKeyListOfInt16TrieAble) "propKeyListOfInt16TrieAble" ["list of ints"] [] undefined,
                 Test $ TestInstance (runQuickCheck propKeyListOfInt32TrieAble) "propKeyListOfInt32TrieAble" ["list of ints"] [] undefined,
                 Test $ TestInstance (runQuickCheck propKeyListOfInt64TrieAble) "propKeyListOfInt64TrieAble" ["list of ints"] [] undefined,
                 Test $ TestInstance (runQuickCheck propTrieOfKeyListOfInt16Sorted) "propTrieOfKeyListOfInt16Sorted" ["list of ints"] [] undefined,
                 Test $ TestInstance (runQuickCheck propTrieOfKeyListOfInt32Sorted) "propTrieOfKeyListOfInt32Sorted" ["list of ints"] [] undefined,
                 Test $ TestInstance (runQuickCheck propTrieOfKeyListOfInt64Sorted) "propTrieOfKeyListOfInt64Sorted" ["list of ints"] [] undefined
                 ]

                 