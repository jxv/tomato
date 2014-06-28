{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Tomato where

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


-- | Time's for (Pomodoro, Short break, Long break)
type Group = (Int, Int, Int)


data Tomato = Tomato
  { _groups  :: [Group]
  , _iterations :: Int
  , _session    :: Session
  } deriving (Show, Eq)


data Session = Session
  { _group     :: Int
  , _iteration :: Int
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
  { _groups = groupsDef
  , _iterations = 4
  , _session = mkSession 0 }


groupsDef :: [Group]
groupsDef =
  [ (25,5,15)
  , (25,3,15)
  , (25,5,30)
  , (25,3,30) ]


mkSession :: Int -> Session
mkSession s = Session
    { _group = s
    , _iteration = 0
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


sessionStep :: Double -> SessionM ()
sessionStep time_limit =
  do t <- use timer
     case t of
       NotStarted  -> return ()
       Finished    -> return ()
       Paused{}    -> return ()
       Running{..} -> do cur_time <- io getCurrentTime
                         diff <- io $ getDiffSeconds runningTime cur_time
                         let secs = runningSeconds + diff
                         timer .= if secs < time_limit
                                     then Running secs cur_time
                                     else Finished
 

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
                           ShortBreak -> interval .= Pomodoro
                           LongBreak  -> do iteration .= 0
                                            interval .= Pomodoro


stepTomato :: Tomato -> IO Tomato
stepTomato tom =
  do let grp = (tom^.groups) !! (tom^.session^.group)
         time_limit = fromIntegral $ case (tom^.session^.interval) of
                        Pomodoro   -> grp^._1
                        ShortBreak -> grp^._2
                        LongBreak  -> grp^._3
     sess <- execStateT (sessionStep (time_limit * 60)) (tom^.session)
     return (set session sess tom)


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


io :: MonadIO m => IO a -> m a
io = liftIO


