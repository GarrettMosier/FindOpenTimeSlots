module Lib where 

import Data.List

-- Custom types
data PointInTime = PointInTime Day Int deriving (Eq, Ord)
newtype Time a   = Time a deriving (Eq, Ord)
data TimeRange a = TimeRange (Time a) (Time a) deriving (Eq, Ord)
type BookablePeriod a = TimeRange a
type OpenSlots a = [TimeRange a]
type Meetings  a = [TimeRange a]

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Ord, Eq, Show)


-- Custom Show instances
instance Show PointInTime where
  show (PointInTime day time) = show day ++ " at " ++ show time

instance Show a => Show (TimeRange a) where
  show (TimeRange beginning end) = show beginning ++ " to " ++ show end

instance Show a => Show (Time a) where
  show (Time x) = show x
  


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
findOpenPeriodsHelper dayRange ((TimeRange meetingStart meetingEnd) : remainingMeetings) openSlots meetingRangeEnd = foundOpenSlots
                where foundOpenSlots = findOpenPeriodsHelper dayRange remainingMeetings newOpenSlots newMeetingRangeEnd
                      toAppendToOpenSlots = meetingStart > meetingRangeEnd
                      newMeetingRangeEnd = max meetingEnd meetingRangeEnd 
                      openSlot = TimeRange meetingRangeEnd meetingStart
                      newOpenSlots = if toAppendToOpenSlots then openSlot : openSlots else openSlots
