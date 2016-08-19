import Data.List

data TimeRange = TimeRange Int Int deriving (Show, Eq, Ord)
type DayPeriod = TimeRange
type OpenSlots = [TimeRange]
type Meetings = [TimeRange]

testRange = [TimeRange 10 12, TimeRange 1 4, TimeRange 19 40, TimeRange 8 9]
testDayRange = TimeRange 0 20


findOpenPeriods :: DayPeriod -> Meetings -> OpenSlots
findOpenPeriods dayRange@(TimeRange startTime endTime) allMeetings@(meeting : remainingMeetings) = reverse openSlots
                where openSlots = findOpenPeriodsHelper dayRange cleanMeetings [] startTime
                      cleanMeetings = filter withinDay $ sort allMeetings
                      withinDay (TimeRange start end) = end > startTime && start < endTime -- Double check semantics for being in a day
                      

findOpenPeriodsHelper :: DayPeriod -> Meetings -> OpenSlots -> Int -> OpenSlots
findOpenPeriodsHelper _ [] openSlots _ = openSlots
findOpenPeriodsHelper dayRange@(TimeRange dayStartTime dayEndTime) (meeting@(TimeRange meetingStart meetingEnd) : remainingMeetings) openSlots meetingRangeEnd = remainingOpenMeetings
                where remainingOpenMeetings = findOpenPeriodsHelper dayRange remainingMeetings newOpenSlots newMeetingRangeEnd
                      toAppend = meetingEnd > meetingRangeEnd
                      newMeetingRangeEnd = meetingEnd
                      openSlot = TimeRange meetingRangeEnd meetingStart
                      newOpenSlots = if toAppend then openSlot : openSlots else openSlots

main = print $ findOpenPeriods testDayRange testRange