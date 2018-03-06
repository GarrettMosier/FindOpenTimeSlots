import Data.List

newtype Time   = Time Int deriving (Show, Eq, Ord)
data TimeRange = TimeRange Time Time deriving (Show, Eq, Ord)
type DayPeriod = TimeRange
type OpenSlots = [TimeRange]
type Meetings  = [TimeRange]


testRangeOne :: Meetings
testRangeOne = [TimeRange (Time 10) (Time 12), TimeRange (Time 1) (Time 4), TimeRange (Time 18) (Time 19), TimeRange (Time 8) (Time 9), TimeRange (Time 4) (Time 25), TimeRange (Time (-5)) (Time 1)]

testRangeTwo :: Meetings
testRangeTwo = [TimeRange (Time 5) (Time 15), TimeRange (Time 1) (Time 2), TimeRange (Time 18) (Time 19), TimeRange (Time 10) (Time 13)]

testRangeThree :: Meetings
testRangeThree = [TimeRange (Time 2) (Time 4), TimeRange (Time 3) (Time 5)]

testDayRange :: DayPeriod
testDayRange = TimeRange (Time 0) (Time 20)


validTimeRange :: TimeRange -> Bool
validTimeRange (TimeRange start end) = start < end && start /= end


clampTimeToDay :: DayPeriod -> TimeRange -> TimeRange
clampTimeToDay (TimeRange dayStart dayEnd) (TimeRange start end) = (TimeRange (max start dayStart) (min end dayEnd))


findOpenPeriods :: DayPeriod -> Meetings -> OpenSlots
findOpenPeriods dayRange@(TimeRange dayStart _) allMeetings = reverse $ filter validTimeRange openSlots
                where openSlots = findOpenPeriodsHelper dayRange validMeetings [] dayStart
                      validMeetings = filter validTimeRange $ sort remappedMeetings
                      remappedMeetings = map (clampTimeToDay dayRange) allMeetings


findOpenPeriodsHelper :: DayPeriod -> Meetings -> OpenSlots -> Time -> OpenSlots
findOpenPeriodsHelper (TimeRange _ dayEnd) [] openSlots lastMeetingEnd = if lastMeetingEnd < dayEnd
                                                                             then (TimeRange lastMeetingEnd dayEnd) : openSlots
                                                                             else openSlots
findOpenPeriodsHelper dayRange ((TimeRange meetingStart meetingEnd) : remainingMeetings) openSlots meetingRangeEnd = remainingOpenMeetings
                where remainingOpenMeetings = findOpenPeriodsHelper dayRange remainingMeetings newOpenSlots newMeetingRangeEnd
                      toAppendToOpenSlots = meetingEnd > meetingRangeEnd 
                      newMeetingRangeEnd = max meetingEnd meetingRangeEnd 
                      openSlot = TimeRange meetingRangeEnd meetingStart
                      newOpenSlots = if toAppendToOpenSlots then openSlot : openSlots else openSlots


main :: IO ()
main = do
  print $ findOpenPeriods testDayRange testRangeOne
  print $ findOpenPeriods testDayRange testRangeTwo
  print $ findOpenPeriods testDayRange testRangeThree
