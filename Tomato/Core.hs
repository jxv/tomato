{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Tomato.Core where

import Control.Monad.State
import Data.Time.Clock
import Data.Time.Calendar
import Control.Lens


data Interval
  = Pomodoro
  | ShortBreak
  | LongBreak
  deriving (Show, Eq)


data Nudger
  = Start
  | Resume
  | Pause
  | Next
  | Restart
  deriving (Show, Eq)


data Timer
  = NotStarted
  | Running { runningSeconds :: Double, runningTime :: UTCTime }
  | Paused { pausedSeconds :: Double }
  | Finished
  deriving (Show, Eq)


data Tomato = Tomato
  { _pomodoro   :: Int
  , _shortBreak :: Int
  , _longBreak  :: Int
  , _iterations :: Int
  , _completed  :: Int
  , _session    :: Session
  } deriving (Show, Eq)


data Session = Session
  { _iteration :: Int
  , _interval  :: Interval
  , _timer     :: Timer
  } deriving (Show, Eq)


makeLenses ''Tomato
makeLenses ''Session


type TomatoM = StateT Tomato IO
type SessionM = StateT Session IO


--


tomatoDef :: Tomato
tomatoDef = Tomato
  { _pomodoro = 25
  , _shortBreak = 5
  , _longBreak = 15
  , _iterations = 4
  , _completed = 0
  , _session = mkSession }


mkSession :: Session
mkSession = Session
    { _iteration = 0
    , _interval = Pomodoro
    , _timer = NotStarted }


getDiffSeconds :: UTCTime -> UTCTime -> IO Double
getDiffSeconds start end =
  do let days_passed = fromIntegral $ (day_no end) - (day_no start)
         secs_diff = (utctDayTime end) - (utctDayTime start)
         secs_dbl = (fromIntegral $ fromEnum secs_diff) / (10^12)
     return (days_passed * secs_per_day + secs_dbl)
 where 
  day_no = toModifiedJulianDay . utctDay
  secs_per_day = 24 * 60 * 60


sessionStep :: Double -> SessionM Bool
sessionStep time_limit =
  do t <- use timer
     case t of
       NotStarted  -> return False
       Finished    -> return False
       Paused{}    -> return False
       Running{..} -> do cur_time <- io getCurrentTime
                         diff <- io $ getDiffSeconds runningTime cur_time
                         let secs = runningSeconds + diff
                         timer .= if secs < time_limit
                                     then Running secs cur_time
                                     else Finished
                         t <- use timer
                         int <- use interval
                         return (t == Finished && int == Pomodoro)
 

sessionNudgeTimer :: Int -> SessionM ()
sessionNudgeTimer iter_limit =
  do t <- use timer
     time <- io getCurrentTime
     case t of
       NotStarted  -> timer .= Running 0 time
       Paused{..}  -> timer .= Running pausedSeconds time
       Running{..} -> timer .= Paused runningSeconds
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


stepTomato :: Tomato -> IO Tomato
stepTomato tom =
  do let time_limit = tomatoTimeLimit tom
     (finished, sess) <- runStateT (sessionStep (time_limit * 60)) (tom^.session)
     return $ (set session sess) (over completed (if finished then (+1) else id) tom)


tomatoTimeLimit :: Num a => Tomato -> a
tomatoTimeLimit tom =  fromIntegral $ 
  tom^.(case (tom^.session^.interval) of
    Pomodoro   -> pomodoro
    ShortBreak -> shortBreak
    LongBreak  -> longBreak)


nudgeTomatoTimer :: Tomato -> IO Tomato
nudgeTomatoTimer tom =
  do sess <- execStateT (sessionNudgeTimer (tom^.iterations)) (tom^.session)
     return (set session sess tom)


nudger :: Tomato -> Nudger
nudger tom = case (tom^.session^.timer) of
  NotStarted -> Start
  Running{}  -> Pause
  Paused{}   -> Resume
  Finished   -> if tom^.session^.interval == LongBreak
                   then Restart
                   else Next


tomatoSeconds :: Tomato -> Double
tomatoSeconds tom =  case (tom^.session^.timer) of
  NotStarted  -> 0
  Paused s    -> s
  Running s _ -> s
  Finished    -> tomatoTimeLimit tom


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


limitSecondsForTimerByMinutes :: Tomato -> Double -> Double
limitSecondsForTimerByMinutes tom minutes =
  let time_limit = tomatoTimeLimit tom
  in if | minutes < 0          -> 0
        | minutes > time_limit -> time_limit * 60
        | otherwise            -> minutes * 60 


limitIterationForTomato :: Tomato -> Int -> Int
limitIterationForTomato tom iter
  | tom^.iterations < iter = tom^.iterations
  | iter < 0               = 0
  | otherwise              = iter


--


io :: MonadIO m => IO a -> m a
io = liftIO


