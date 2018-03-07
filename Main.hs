import Lib 

testRangeOne :: Meetings Int
testRangeOne = [TimeRange (Time 10) (Time 12), TimeRange (Time 1) (Time 4), TimeRange (Time 18) (Time 19), TimeRange (Time 8) (Time 9), TimeRange (Time 4) (Time 25), TimeRange (Time (-5)) (Time 1)]

testRangeTwo :: Meetings Int
testRangeTwo = [TimeRange (Time 5) (Time 15), TimeRange (Time 1) (Time 2), TimeRange (Time 18) (Time 19), TimeRange (Time 10) (Time 13)]

testRangeThree :: Meetings Int
testRangeThree = [TimeRange (Time 2) (Time 4), TimeRange (Time 3) (Time 5)]

testWeekOne :: Meetings Day
testWeekOne = [TimeRange (Time Sunday) (Time Tuesday), TimeRange (Time Thursday) (Time Friday)]

testFullWeekOne :: Meetings PointInTime
testFullWeekOne = [TimeRange (Time (PointInTime Sunday 2)) (Time (PointInTime Monday 4)), TimeRange (Time (PointInTime Tuesday 3)) (Time (PointInTime Thursday 5))]


-- Min and Max values for test values
testDayRange :: BookablePeriod Int
testDayRange = TimeRange (Time 0) (Time 24)

testWeekRange :: BookablePeriod Day
testWeekRange = TimeRange (Time Sunday) (Time Saturday)

testFullWeekRange :: BookablePeriod PointInTime
testFullWeekRange = TimeRange (Time (PointInTime Sunday 0)) (Time (PointInTime Saturday 24))


-- Expected results from the tests
testRangeOneAnswer    = []
testRangeTwoAnswer    = [TimeRange (Time 0) (Time 1), TimeRange (Time 2) (Time 5), TimeRange (Time 15) (Time 18), TimeRange (Time 19) (Time 24)]
testRangeThreeAnswer  = [TimeRange (Time 0) (Time 2), TimeRange (Time 5) (Time 24)]
testWeekOneAnswer     = [TimeRange (Time Tuesday) (Time Thursday), TimeRange (Time Friday) (Time Saturday)]
testFullWeekOneAnswer = [TimeRange (Time (PointInTime Sunday 0)) (Time (PointInTime Sunday 2)), TimeRange (Time (PointInTime Monday 4)) (Time (PointInTime Tuesday 3)), TimeRange (Time (PointInTime Thursday 5)) (Time (PointInTime Saturday 24))]


main :: IO ()
main = do
  print $ findOpenPeriods testDayRange testRangeOne
  print $ findOpenPeriods testDayRange testRangeTwo
  print $ findOpenPeriods testDayRange testRangeThree
  print $ findOpenPeriods testWeekRange testWeekOne
  print $ findOpenPeriods testFullWeekRange testFullWeekOne

  print $ findOpenPeriods testDayRange testRangeOne == testRangeOneAnswer
  print $ findOpenPeriods testDayRange testRangeTwo == testRangeTwoAnswer
  print $ findOpenPeriods testDayRange testRangeThree == testRangeThreeAnswer
  print $ findOpenPeriods testWeekRange testWeekOne == testWeekOneAnswer
  print $ findOpenPeriods testFullWeekRange testFullWeekOne == testFullWeekOneAnswer
