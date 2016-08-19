import Data.List

data TimeRange = TimeRange Int Int deriving (Show, Eq, Ord)
type DayPeriod = TimeRange
type OpenSlots = [TimeRange]
type Meetings = [TimeRange]

testRange :: Meetings
testRange = [TimeRange 10 12, TimeRange 1 4, TimeRange 19 40, TimeRange 8 9, TimeRange 0 22]

testDayRange :: DayPeriod
testDayRange = TimeRange 0 20


findOpenPeriods :: DayPeriod -> Meetings -> OpenSlots
findOpenPeriods dayRange@(TimeRange dayStart dayEnd) allMeetings = reverse $ filter validTimeRange openSlots
                where openSlots = findOpenPeriodsHelper dayRange cleanMeetings [] dayStart
                      cleanMeetings = filter withinDay $ sort allMeetings
                      withinDay (TimeRange start end) = start < dayEnd && end > dayStart -- Double check semantics for being in a day
                      validTimeRange (TimeRange start end) = start < end

findOpenPeriodsHelper :: DayPeriod -> Meetings -> OpenSlots -> Int -> OpenSlots
findOpenPeriodsHelper _ [] openSlots _ = openSlots
findOpenPeriodsHelper dayRange ((TimeRange meetingStart meetingEnd) : remainingMeetings) openSlots meetingRangeEnd = remainingOpenMeetings
                where remainingOpenMeetings = findOpenPeriodsHelper dayRange remainingMeetings newOpenSlots newMeetingRangeEnd
                      toAppend = meetingEnd > meetingRangeEnd
                      newMeetingRangeEnd = max meetingEnd meetingRangeEnd
                      openSlot = TimeRange meetingRangeEnd meetingStart
                      newOpenSlots = if toAppend then openSlot : openSlots else openSlots


main :: IO ()
main = print $ findOpenPeriods testDayRange testRange
