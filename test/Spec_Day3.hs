module Spec_Day3 where
import Test.HUnit
import Data.List
import Day3

testString = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"
testMatrix = [ [0,0,1,0,0], 
            [1,1,1,1,0], 
            [1,0,1,1,0], 
            [1,0,1,1,1], 
            [1,0,1,0,1], 
            [0,1,1,1,1], 
            [0,0,1,1,1], 
            [1,1,1,0,0], 
            [1,0,0,0,0], 
            [1,1,0,0,1], 
            [0,0,0,1,0], 
            [0,1,0,1,0] ]

o2genr_index_0 = [ [1,1,1,1,0],
            [1,0,1,1,0], 
            [1,0,1,1,1], 
            [1,0,1,0,1], 
            [1,1,1,0,0], 
            [1,0,0,0,0], 
            [1,1,0,0,1] ]

o2genr_index_1 = [ [1,0,1,1,0],
            [1,0,1,1,1],
            [1,0,1,0,1],
            [1,0,0,0,0]]

testGammaValues = [1,0,1,1,0]
testEpsilonValue = [0,1,0,0,1]

testStringToListOfBits = TestCase(assertEqual "1001" testMatrix (linesToListsOfListOfBits testString))
testGammas = TestCase(assertEqual "1,0,1,1,0" testGammaValues (gammas $ transpose testMatrix))
testEpsilons = TestCase(assertEqual "0,1,0,0,1" testEpsilonValue (epsilons $ transpose testMatrix))
testBinToDec = TestCase(assertEqual "binary 22" 22 (binToDec testGammaValues))

testGetOxygenRating = TestCase(assertEqual "oxygen rating" [1,0,1,1,1] (getOxygenRating testMatrix)) 
testGetScrubberRating = TestCase(assertEqual "scrubber rating" [0,1,0,1,0] (getScrubberRating testMatrix)) 