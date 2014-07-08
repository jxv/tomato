{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tomato.Core
  ( module Tomato.Core.Types
  , tomatoDef
  , stepTomato
  , tomatoTimeLimit
  , nudgeTomatoTimer
  , nudger
  , tomatoSeconds
  , startRing
  , stopRing
  , startTickTock
  , stopTickTock
  , limitSecondsForTimerByMinutes
  , toSeconds
  , toMinutes
  , startingLastMinute
  , startingEveryNthMinutes
  ) where

import Control.Monad.State
import Data.Time.Clock
import Data.Time.Calendar
import Control.Lens

import Tomato.Core.Types

--

type TomatoM = State Tomato

--

tomatoDef :: Tomato
tomatoDef = Tomato
  { _pomodoro = Minutes 25
  , _shortBreak = Minutes 5
  , _longBreak = Minutes 15
  , _iterations = 4
  , _completed = 0
  , _iteration = 0
  , _interval = Pomodoro
  , _timer = NotStarted }

getDiffSeconds :: UTCTime -> UTCTime -> Seconds
getDiffSeconds start end =
  let days_passed = fromIntegral $ (day_no end) - (day_no start)
      secs_diff = (utctDayTime end) - (utctDayTime start)
      secs_dbl = (fromIntegral $ fromEnum secs_diff) / (10^12)
  in Seconds (days_passed * secs_per_day + secs_dbl)
 where 
  day_no = toModifiedJulianDay . utctDay
  secs_per_day = 24 * 60 * 60

tomatoStep :: UTCTime -> TomatoM ()
tomatoStep cur_time =
  do t <- use timer
     case t of
       NotStarted          -> return ()
       Finished            -> return ()
       Paused{}            -> return ()
       Running s prev_time ->
         do let diff = getDiffSeconds prev_time cur_time
                secs = s + diff
            time_limit <- use (to tomatoTimeLimit)
            timer .= if secs < (toSeconds time_limit)
                        then Running secs cur_time
                        else Finished
            t <- use timer
            int <- use interval
            completed %= if (t == Finished && int == Pomodoro) then (+1) else id

tomatoNudgeTimer :: UTCTime -> TomatoM ()
tomatoNudgeTimer cur_time =
  do t <- use timer
     iter_limit <- use iterations
     case t of
       NotStarted  -> timer .= Running (Seconds 0) cur_time
       Paused s    -> timer .= Running s cur_time
       Running s _ -> timer .= Paused s
       Finished    -> do timer .= NotStarted
                         int <- use interval
                         case int of
                           Pomodoro   -> do iteration %= (+1)
                                            iter <- use iteration
                                            interval .= if iter < iter_limit
                                                           then ShortBreak
                                                           else LongBreak
                           ShortBreak -> do iter <- use iteration
                                            iteration .= if iter < iter_limit
                                                            then iter
                                                            else 0
                                            interval .= Pomodoro
                           LongBreak  -> do iteration .= 0
                                            interval .= Pomodoro

stepTomato :: Tomato -> UTCTime -> Tomato
stepTomato tom cur_time = execState (tomatoStep cur_time) tom

tomatoTimeLimit :: Tomato -> Minutes
tomatoTimeLimit tom =
  tom^.(case (tom^.interval) of
    Pomodoro   -> pomodoro
    ShortBreak -> shortBreak
    LongBreak  -> longBreak)

nudgeTomatoTimer :: Tomato -> UTCTime -> Tomato
nudgeTomatoTimer tom cur_time = execState (tomatoNudgeTimer cur_time) tom

nudger :: Tomato -> Nudger
nudger tom = case (tom^.timer) of
  NotStarted -> Start
  Running{}  -> Pause
  Paused{}   -> Resume
  Finished   -> Next

tomatoSeconds :: Tomato -> Seconds
tomatoSeconds tom =  case (tom^.timer) of
  NotStarted  -> 0
  Paused s    -> s
  Running s _ -> s
  Finished    -> toSeconds (tomatoTimeLimit tom)

startRing :: Timer -> Timer -> Bool
startRing Running{} Finished = True
startRing Paused{}  Finished = True
startRing _         _        = False

stopRing :: Timer -> Timer -> Bool
stopRing Finished Finished = False
stopRing Finished        _ = True
stopRing _               _ = False

startTickTock :: Timer -> Timer -> Bool
startTickTock NotStarted Running{} = True
startTickTock Paused{}   Running{} = True
startTickTock _          _         = False

stopTickTock :: Timer -> Timer -> Bool
stopTickTock Running{}  Running{}  = False
stopTickTock Running{}  _          = True
stopTickTock _          _          = False

-- Auxiliary functions for adjusting data from outside tomato-core.

limitSecondsForTimerByMinutes :: Tomato -> Minutes -> Seconds
limitSecondsForTimerByMinutes tom mins =
  let time_limit = tomatoTimeLimit tom
  in if | mins < 0          -> Seconds 0
        | mins > time_limit -> toSeconds time_limit
        | otherwise         -> toSeconds mins

startingLastMinute :: Minutes -> Seconds -> Seconds -> Bool
startingLastMinute max_mins secs secs' = 
  let max_secs   = toSeconds max_mins
      secs_left  = max_secs - secs
      secs_left' = max_secs - secs'
  in secs_left > 60 && secs_left' <= 60 && secs_left' > 0

startingEveryNthMinutes :: Minutes -> Seconds -> Seconds -> Bool
startingEveryNthMinutes int_mins secs secs' = 
  let int_mins' = round int_mins
      mid_min   = floor (toMinutes secs')
      is_mid    = floor (toMinutes secs) < mid_min
  in is_mid && (mid_min  `mod` int_mins' == 0)

--

toSeconds :: Minutes -> Seconds
toSeconds (Minutes m) = Seconds (m * 60)

toMinutes :: Seconds -> Minutes
toMinutes (Seconds s) = Minutes (s / 60)

