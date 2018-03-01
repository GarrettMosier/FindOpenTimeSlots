import Data.List

data TimeRange = TimeRange Time Time deriving (Show, Eq, Ord)
type DayPeriod = TimeRange
type OpenSlots = [TimeRange]
type Meetings = [TimeRange]
type Time = Int

testRange :: Meetings
testRange = [TimeRange 10 12, TimeRange 1 4, TimeRange 18 19, TimeRange 8 9, TimeRange 4 25, TimeRange (-5) 1]

testDayRange :: DayPeriod
testDayRange = TimeRange 0 20


findOpenPeriods :: DayPeriod -> Meetings -> OpenSlots
findOpenPeriods dayRange@(TimeRange dayStart dayEnd) allMeetings = reverse $ filter validTimeRange openSlots
                where openSlots = findOpenPeriodsHelper dayRange cleanMeetings [] dayStart
                      cleanMeetings = filter withinDay $ sort allMeetings
                      withinDay (TimeRange start end) = start < dayEnd && end > dayStart -- Double check semantics for being in a day
                      validTimeRange (TimeRange start end) = start < end

findOpenPeriodsHelper :: DayPeriod -> Meetings -> OpenSlots -> Time -> OpenSlots
findOpenPeriodsHelper (TimeRange _ dayEnd) [] openSlots lastMeetingEnd = if lastMeetingEnd < dayEnd then (TimeRange lastMeetingEnd dayEnd) : openSlots else openSlots
findOpenPeriodsHelper dayRange ((TimeRange meetingStart meetingEnd) : remainingMeetings) openSlots meetingRangeEnd = remainingOpenMeetings
                where remainingOpenMeetings = findOpenPeriodsHelper dayRange remainingMeetings newOpenSlots newMeetingRangeEnd
                      toAppend = meetingEnd > meetingRangeEnd
                      newMeetingRangeEnd = max meetingEnd meetingRangeEnd
                      openSlot = TimeRange meetingRangeEnd meetingStart
                      newOpenSlots = if toAppend then openSlot : openSlots else openSlots


main :: IO ()
main = print $ findOpenPeriods testDayRange testRange
