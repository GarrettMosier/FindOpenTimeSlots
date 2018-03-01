import Data.List

data TimeRange = TimeRange Time Time deriving (Show, Eq, Ord)
type DayPeriod = TimeRange
type OpenSlots = [TimeRange]
type Meetings = [TimeRange]
type Time = Int

testRangeOne :: Meetings
testRangeOne = [TimeRange 10 12, TimeRange 1 4, TimeRange 18 19, TimeRange 8 9, TimeRange 4 25, TimeRange (-5) 1]

testRangeTwo :: Meetings
testRangeTwo = [TimeRange 5 15, TimeRange 1 2, TimeRange 18 19, TimeRange 10 13]

testRangeThree :: Meetings
testRangeThree = [TimeRange 2 4, TimeRange 3 5]


testDayRange :: DayPeriod
testDayRange = TimeRange 0 20


validTimeRange :: TimeRange -> Bool
validTimeRange (TimeRange start end) = start < end


findOpenPeriods :: DayPeriod -> Meetings -> OpenSlots
findOpenPeriods dayRange@(TimeRange dayStart dayEnd) allMeetings = reverse $ filter validTimeRange openSlots
                where openSlots = findOpenPeriodsHelper dayRange meetingsDuringDay [] dayStart
                      meetingsDuringDay = filter withinDay $ sort allMeetings
                      withinDay (TimeRange start end) = start < dayEnd && end > dayStart


findOpenPeriodsHelper :: DayPeriod -> Meetings -> OpenSlots -> Time -> OpenSlots
findOpenPeriodsHelper (TimeRange _ dayEnd) [] openSlots lastMeetingEnd = if lastMeetingEnd < dayEnd
                                                                             then (TimeRange lastMeetingEnd dayEnd) : openSlots
                                                                             else openSlots
findOpenPeriodsHelper dayRange ((TimeRange meetingStart meetingEnd) : remainingMeetings) openSlots meetingRangeEnd = remainingOpenMeetings
                where remainingOpenMeetings = findOpenPeriodsHelper dayRange remainingMeetings newOpenSlots newMeetingRangeEnd
                      toAppend = meetingEnd > meetingRangeEnd
                      newMeetingRangeEnd = max meetingEnd meetingRangeEnd
                      openSlot = TimeRange meetingRangeEnd meetingStart
                      newOpenSlots = if toAppend then openSlot : openSlots else openSlots


main :: IO ()
main = do
  print $ findOpenPeriods testDayRange testRangeOne
  print $ findOpenPeriods testDayRange testRangeTwo
  print $ findOpenPeriods testDayRange testRangeThree
