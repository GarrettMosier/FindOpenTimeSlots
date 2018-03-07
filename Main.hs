import Data.List

data PointInTime = PointInTime Day Int deriving (Show, Eq, Ord)
newtype Time a   = Time a deriving (Show, Eq, Ord)
data TimeRange a = TimeRange (Time a) (Time a) deriving (Show, Eq, Ord)
type BookablePeriod a = TimeRange a
type OpenSlots a = [TimeRange a]
type Meetings  a = [TimeRange a]

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Ord, Eq, Show)


testRangeOne :: Meetings Int
testRangeOne = [TimeRange (Time 10) (Time 12), TimeRange (Time 1) (Time 4), TimeRange (Time 18) (Time 19), TimeRange (Time 8) (Time 9), TimeRange (Time 4) (Time 25), TimeRange (Time (-5)) (Time 1)]

testRangeTwo :: Meetings Int
testRangeTwo = [TimeRange (Time 5) (Time 15), TimeRange (Time 1) (Time 2), TimeRange (Time 18) (Time 19), TimeRange (Time 10) (Time 13)]

testRangeThree :: Meetings Int
testRangeThree = [TimeRange (Time 2) (Time 4), TimeRange (Time 3) (Time 5)]

testFullWeekOne :: Meetings PointInTime
testFullWeekOne = [TimeRange (Time (PointInTime Sunday 2)) (Time (PointInTime Monday 4)), TimeRange (Time (PointInTime Tuesday 3)) (Time (PointInTime Thursday 5))]

testWeekOne :: Meetings Day
testWeekOne = [TimeRange (Time Sunday) (Time Tuesday), TimeRange (Time Thursday) (Time Friday)]


testDayRange :: BookablePeriod Int
testDayRange = TimeRange (Time 0) (Time 24)

testWeekRange :: BookablePeriod Day
testWeekRange = TimeRange (Time Sunday) (Time Saturday)

testFullWeekRange :: BookablePeriod PointInTime
testFullWeekRange = TimeRange (Time (PointInTime Sunday 0)) (Time (PointInTime Saturday 24))





-- Ensures time range makes sense
validTimeRange :: Ord a => TimeRange a -> Bool
validTimeRange (TimeRange start end) = start < end && start /= end


-- Ensures meeting time is within the course of a day
clampTimeToDay :: Ord a => BookablePeriod a -> TimeRange a -> TimeRange a
clampTimeToDay (TimeRange dayStart dayEnd) (TimeRange start end) = (TimeRange (max start dayStart) (min end dayEnd))


-- Finds all time during day where a meeting isn't scheduled
findOpenPeriods :: Ord a => BookablePeriod a -> Meetings a -> OpenSlots a
findOpenPeriods dayRange@(TimeRange dayStart _) allMeetings = reverse openSlots
                where openSlots = findOpenPeriodsHelper dayRange validMeetings [] dayStart
                      validMeetings = filter validTimeRange $ sort remappedMeetings
                      remappedMeetings = map (clampTimeToDay dayRange) allMeetings


findOpenPeriodsHelper :: Ord a => BookablePeriod a -> Meetings a -> OpenSlots a -> Time a -> OpenSlots a
findOpenPeriodsHelper (TimeRange _ dayEnd) [] openSlots lastMeetingEnd = if lastMeetingEnd < dayEnd
                                                                             then (TimeRange lastMeetingEnd dayEnd) : openSlots
                                                                             else openSlots
findOpenPeriodsHelper dayRange ((TimeRange meetingStart meetingEnd) : remainingMeetings) openSlots meetingRangeEnd = remainingOpenMeetings
                where remainingOpenMeetings = findOpenPeriodsHelper dayRange remainingMeetings newOpenSlots newMeetingRangeEnd
                      toAppendToOpenSlots = meetingStart > meetingRangeEnd
                      newMeetingRangeEnd = max meetingEnd meetingRangeEnd 
                      openSlot = TimeRange meetingRangeEnd meetingStart
                      newOpenSlots = if toAppendToOpenSlots then openSlot : openSlots else openSlots


main :: IO ()
main = do
  print $ findOpenPeriods testDayRange testRangeOne
  print $ findOpenPeriods testDayRange testRangeTwo
  print $ findOpenPeriods testDayRange testRangeThree
  print $ findOpenPeriods testWeekRange testWeekOne
  print $ findOpenPeriods testFullWeekRange testFullWeekOne
