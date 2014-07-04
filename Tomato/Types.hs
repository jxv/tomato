{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tomato.Types where


import Control.Monad.State
import Data.Time.Clock
import Data.Time.Calendar
import Control.Lens


newtype Seconds = Seconds { seconds :: Double }
          deriving (Show, Eq, Num, Floating, Ord, Fractional, Real, RealFloat, RealFrac)

newtype Minutes = Minutes { minutes :: Double }
          deriving (Show, Eq, Num, Floating, Ord, Fractional, Real, RealFloat, RealFrac)


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
  deriving (Show, Eq)


data Timer
  = NotStarted
  | Running Seconds UTCTime
  | Paused Seconds
  | Finished
  deriving (Show, Eq)


data Tomato = Tomato
  { _pomodoro   :: Minutes
  , _shortBreak :: Minutes
  , _longBreak  :: Minutes
  , _iterations :: Int
  , _completed  :: Int
  , _iteration  :: Int
  , _interval   :: Interval
  , _timer      :: Timer
  } deriving (Show, Eq)


makeLenses ''Tomato


