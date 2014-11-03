{-# LANGUAGE PackageImports, RecordWildCards, NamedFieldPuns #-}
module TestDetailed (tests) where

import qualified Test.QuickCheck as Q
import Distribution.TestSuite as TS

import TestTrieAble

toTSResult :: Q.Result -> TS.Result
toTSResult Q.Success {} = TS.Pass
toTSResult Q.GaveUp {} = TS.Fail "GaveUp"
toTSResult Q.Failure {reason} = TS.Fail reason


runQuickCheck :: Q.Testable p => p -> IO TS.Progress
runQuickCheck prop = do
        qres <- Q.quickCheckWithResult Q.stdArgs {Q.maxSuccess = 40, Q.maxSize = 40} prop
        return $ (Finished . toTSResult) qres
        
tests :: IO [Test]
tests = return [ Test $ TestInstance (runQuickCheck propSorted) "propSorted" ["set"] [] undefined
                 ]