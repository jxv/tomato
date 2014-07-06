{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Tomato.Ui.Types where


import FRP.Sodium
import Control.Lens 
import qualified DBus.Notify as N
import qualified Graphics.UI.SDL       as S
import qualified Graphics.UI.SDL.Mixer as S
import Graphics.UI.Gtk.Gdk.Gdk
import Graphics.UI.Gtk

import Tomato.Core.Types


newtype Volume = Volume { volume :: Int }
          deriving (Enum, Eq, Integral, Num, Ord, Read, Real, Show)


data Ui = Ui
  { _window                       :: Window
  , _timerIntervalLabel           :: Label
  , _timerCompletedLabel          :: Label
  , _timerMinutesScale            :: Scale
  , _timerMinutesAdjustment       :: Adjustment
  , _timerNudgeButton             :: Button 
  , _settingsPomodoroSpinButton   :: SpinButton
  , _settingsShortSpinButton      :: SpinButton
  , _settingsLongSpinButton       :: SpinButton
  , _settingsIterationsSpinButton :: SpinButton
  , _settingsVolumeScale          :: Scale
  , _settingsPomodoroAdjustment   :: Adjustment
  , _settingsShortAdjustment      :: Adjustment
  , _settingsLongAdjustment       :: Adjustment
  , _settingsIterationsAdjustment :: Adjustment
  , _settingsVolumeAdjustment     :: Adjustment
  , _notifierClient               :: N.Client }


data Frp = Frp
  { _timerNudgeEvent         :: Event (App -> IO App)
  , _timerNudgeCb            :: Reactive ()
  , _timerMinutesEvent       :: Event (App -> IO App)
  , _timerMinutesCb          :: Double -> Reactive ()
  , _settingsPomodoroEvent   :: Event (App -> IO App)
  , _settingsPomodoroCb      :: Double -> Reactive ()
  , _settingsShortBreakEvent :: Event (App -> IO App)
  , _settingsShortBreakCb    :: Double -> Reactive ()
  , _settingsLongBreakEvent  :: Event (App -> IO App)
  , _settingsLongBreakCb     :: Double -> Reactive ()
  , _settingsIterationsEvent :: Event (App -> IO App) 
  , _settingsIterationsCb    :: Double -> Reactive ()
  , _settingsVolumeEvent     :: Event (App -> IO App)
  , _settingsVolumeCb        :: Double -> Reactive () }


data AudioRes = AudioRes
  { _tickTockMusic :: S.Music
  , _ringMusic     :: S.Music }


data App = App
  { _tomato    :: Tomato
  , _ui        :: Ui
  , _frp       :: Frp
  , _audioRes  :: AudioRes
  , _sfxVolume :: Volume }


makeLenses ''Ui
makeLenses ''Frp
makeLenses ''AudioRes
makeLenses ''App


