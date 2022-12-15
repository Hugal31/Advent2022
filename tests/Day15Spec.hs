module Day15Spec (spec) where

import Advent2022.Day15
import Advent2022.ParseUtils

import Data.Range
import Test.Hspec

spec :: Spec
spec = do
    describe "parse" $ do
        it "should parse sensor" $ do
            execParser parseSensor "Sensor at x=2, y=18: closest beacon is at x=-2, y=15" `shouldBe` Sensor (2, 18) (-2, 15)

    describe "sensor" $ do
        it "should compute the sensor range" $ do
            sensorRange (Sensor (0, 0) (5, 5)) `shouldBe` 10
            sensorRange (Sensor (0, 5) (5, -15)) `shouldBe` 25

        it "should tell if in sensor range" $ do
            inSensorRange (Sensor (0, 0) (5, 5)) (5, 5) `shouldBe` True
            inSensorRange (Sensor (0, 0) (5, 5)) (2, 5) `shouldBe` True
            inSensorRange (Sensor (0, 0) (5, 5)) (-5, -5) `shouldBe` True
            inSensorRange (Sensor (0, 0) (5, 5)) (-7, -5) `shouldBe` False
            inSensorRange (Sensor (100, 0) (5, 5)) (150, 8) `shouldBe` True

        it "doesSensorTellEmpty should tell if a cell is empty" $ do
            doesSensorTellEmpty (Sensor (0, 0) (5, 5)) (5, 5) `shouldBe` Just False
            doesSensorTellEmpty (Sensor (0, 0) (5, 5)) (4, 5) `shouldBe` Just True
            doesSensorTellEmpty (Sensor (0, 0) (5, 5)) (6, 5) `shouldBe` Nothing

        it "doSensorsTellEmpty should tell if a cell is empty" $ do
            let sensors = [Sensor (0, 0) (5, 5), Sensor (5, 0) (11, 0)]
                in do
                    doSensorsTellEmpty sensors (5, 5) `shouldBe` Just False
                    doSensorsTellEmpty sensors (11, 0) `shouldBe` Just False
                    doSensorsTellEmpty sensors (2, 2) `shouldBe` Just True
                    doSensorsTellEmpty sensors (15, 0) `shouldBe` Nothing

        it "should guess line range" $ do
            guessLineRange exampleList 10 `shouldBe` (-2, 24)

    describe "ranges" $ do
        it "should return the range of a sensor" $ do
            getSensorRangeAtRow (Sensor (0, 0) (0, 5)) 0 `shouldBe` [(-5) +=+ 5]
            getSensorRangeAtRow (Sensor (0, 0) (0, 5)) 5 `shouldBe` [0 +=+ 0]
            getSensorRangeAtRow (Sensor (0, 0) (0, 5)) 6 `shouldBe` []
            getSensorRangeAtRow (Sensor (0, 0) (0, 5)) 4 `shouldBe` [(-1) +=+ 1]

            getSensorRangeAtRow (Sensor (0, 0) (5, 5)) 0 `shouldBe` [(-10) +=+ 10]
            getSensorRangeAtRow (Sensor (0, 0) (5, 5)) 5 `shouldBe` [(-5) +=+ 5]
            getSensorRangeAtRow (Sensor (0, 0) (5, 5)) 11 `shouldBe` []
            getSensorRangeAtRow (Sensor (0, 0) (5, 5)) 5 `shouldBe` [(-5) +=+ 5]

        it "should return the range of a sensor" $ do
            getSensorEmptyRangeAtRow (Sensor (0, 0) (0, 5)) 5 `shouldBe` []

            getSensorEmptyRangeAtRow (Sensor (0, 0) (5, 5)) 0 `shouldBe` [(-10) +=+ 10]
            getSensorEmptyRangeAtRow (Sensor (0, 0) (5, 5)) 11 `shouldBe` []
            getSensorEmptyRangeAtRow (Sensor (0, 0) (5, 5)) 5 `shouldBe` [(-5) +=* 5]

            getSensorEmptyRangeAtRow (Sensor (14, 3) (15, 3)) 3 `shouldBe` [13 +=* 15]

    describe "rangeSize" $ do
        it "should give range size" $ do
            rangeSize (SingletonRange 42) `shouldBe` Just 1
            rangeSize (1+=+1) `shouldBe` Just 1
            rangeSize (1+=*2) `shouldBe` Just 1
            rangeSize (1*=*2) `shouldBe` Just 0
            rangeSize (1*=*3) `shouldBe` Just 1
            rangeSize (1+=*3) `shouldBe` Just 2
            rangeSize (1+=+2) `shouldBe` Just 2
            rangeSize (lbi 1) `shouldBe` Nothing
            rangeSize (ube 1) `shouldBe` Nothing

    describe "countEmptyInRow" $ do
        it "should solve example" $ do
            countEmptyInRow exampleList 10 `shouldBe` 26

    describe "countEmptyInRowWithRange" $ do
        it "should solve example" $ do
            countEmptyInRowWithRange exampleList 10 `shouldBe` 26

    describe "find unknown spot" $ do
        it "should solve example" $ do
            findUnknownSpot 20 exampleList `shouldBe` (14, 11)

exampleString = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\n\
\Sensor at x=9, y=16: closest beacon is at x=10, y=16\n\
\Sensor at x=13, y=2: closest beacon is at x=15, y=3\n\
\Sensor at x=12, y=14: closest beacon is at x=10, y=16\n\
\Sensor at x=10, y=20: closest beacon is at x=10, y=16\n\
\Sensor at x=14, y=17: closest beacon is at x=10, y=16\n\
\Sensor at x=8, y=7: closest beacon is at x=2, y=10\n\
\Sensor at x=2, y=0: closest beacon is at x=2, y=10\n\
\Sensor at x=0, y=11: closest beacon is at x=2, y=10\n\
\Sensor at x=20, y=14: closest beacon is at x=25, y=17\n\
\Sensor at x=17, y=20: closest beacon is at x=21, y=22\n\
\Sensor at x=16, y=7: closest beacon is at x=15, y=3\n\
\Sensor at x=14, y=3: closest beacon is at x=15, y=3\n\
\Sensor at x=20, y=1: closest beacon is at x=15, y=3\n"

exampleList = parse exampleString
