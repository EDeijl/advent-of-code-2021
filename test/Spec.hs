import Test.HUnit
import System.Exit
import Spec_Day3
import Spec_Day4

main :: IO ()
main = do
  counts <- runTestTT ( test [
    -- day 3
    testStringToListOfBits,
    testGammas,
    testEpsilons,
    testBinToDec,
    testGetOxygenRating,
    testGetScrubberRating,

    -- day 4
    testParseStringToCell,
    testParseStringToRow,
    testParseStringToGame,
    testPlayBingoStep,
    testWinningBoard

    ])
  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure





