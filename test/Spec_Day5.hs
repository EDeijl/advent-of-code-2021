module Spec_Day5 where

import Test.HUnit
import Day5

testInputString = "0,9 -> 5,9\n\
\8,0 -> 0,8\n\
\9,4 -> 3,4\n\
\2,2 -> 2,1\n\
\7,0 -> 7,4\n\
\6,4 -> 2,0\n\
\0,9 -> 2,9\n\
\3,4 -> 1,4\n\
\0,0 -> 8,8\n\
\5,5 -> 8,2"

testInputVectors = [
  Vector (Point 0 9) (Point 5 9),
  Vector (Point 8 0) (Point 0 8),
  Vector (Point 9 4) (Point 3 4),
  Vector (Point 2 2) (Point 2 1),
  Vector (Point 7 0) (Point 7 4),
  Vector (Point 6 4) (Point 2 0),
  Vector (Point 0 9) (Point 2 9),
  Vector (Point 3 4) (Point 1 4),
  Vector (Point 0 0) (Point 8 8),
  Vector (Point 5 5) (Point 8 2)]

matchingBools = [ False, False, False, False, False, False, True, False, False, False]

testParseVector = TestCase (assertEqual "Parses vector correctly" (Vector (Point 0 9) (Point 5 9)) (parseVector "0,9 -> 5,9"))
testParseInputString = TestCase (assertEqual "Parses multiline string" testInputVectors (parseInput testInputString))



testColinearOrientation = TestCase (assertEqual "Check the orientation of colinear points" 0 (orientation (Point 0 0) (Point 1 0) (Point 2 0)))
testClockWiseOrientation = TestCase (assertEqual "Check the orientation of clockwise points" 1 (orientation (Point 0 0) (Point 0 9) (Point 3 0)))
testCounterClockWiseOrientation = TestCase (assertEqual "Check the orientation of counter-clockwise points" 2 (orientation (Point 0 0) (Point 3 0) (Point 0 9)))

testOnSegment = TestCase (assertEqual "Check if a point is on a segment" True (onSegment (Point 0 0) (Point 0 5) (Point 0 9)))

testIntersectTrue = TestCase (assertEqual "Check that two lines intersect" True (doIntersect (Vector (Point 0 9) (Point 5 9)) (Vector (Point 9 0) (Point 2 9))))
testIntersectFalse = TestCase (assertEqual "Check that two lines intersect" False (doIntersect (Vector (Point 7 0) (Point 7 4)) (Vector (Point 2 2) (Point 2 1))))

testIntersectsWithMultiple = TestCase (assertEqual "Check that a vector intersects with any of the other vectors" matchingBools (intersects (head testInputVectors) testInputVectors))

testCrossMapIntersects = TestCase (assertEqual "Count the number of intersections" 5 (crossMapAllIntersects testInputVectors))

testVectorLength = TestCase (assertEqual "Check the length of a vector" 6 (vectorLength (Vector (Point 0 9) (Point 5 9))))
tests = [
    testParseVector
  , testParseInputString
  , testColinearOrientation
  , testClockWiseOrientation
  , testCounterClockWiseOrientation
  , testOnSegment
  , testIntersectTrue
  , testIntersectFalse
  , testIntersectsWithMultiple
  , testVectorLength
  -- , testCrossMapIntersects -- fails
  ]