{-# LANGUAGE TemplateHaskell #-}


module Tomato.Ui.Types where


import FRP.Sodium
import Control.Lens 
import qualified DBus.Notify as N
import qualified Graphics.UI.SDL       as S
import qualified Graphics.UI.SDL.Mixer as S
import Graphics.UI.Gtk.Gdk.Gdk
import Graphics.UI.Gtk

import Tomato.Core.Types


data UiTimer = UiTimer
  { _uiTimerIntervalLabel     :: Label
  , _uiTimerCompletedLabel    :: Label
  , _uiTimerMinutesScale      :: Scale
  , _uiTimerMinutesAdjustment :: Adjustment
  , _uiTimerNudgeButton       :: Button }


data UiSettings = UiSettings
  { _uiSettingsPomodoroSpinButton   :: SpinButton
  , _uiSettingsShortSpinButton      :: SpinButton
  , _uiSettingsLongSpinButton       :: SpinButton
  , _uiSettingsIterationsSpinButton :: SpinButton
  , _uiSettingsVolumeScale          :: Scale
  , _uiSettingsPomodoroAdjustment   :: Adjustment
  , _uiSettingsShortAdjustment      :: Adjustment
  , _uiSettingsLongAdjustment       :: Adjustment
  , _uiSettingsIterationsAdjustment :: Adjustment
  , _uiSettingsVolumeAdjustment     :: Adjustment }


data Ui = Ui
  { _uiWindow   :: Window
  , _uiTimer    :: UiTimer
  , _uiSettings :: UiSettings
  , _UiNotifier :: N.Client }


data Frp = Frp
  { _frpTimerNudgeEvent         :: Event (Tomato -> IO Tomato)
  , _frpTimerNudgeCb            :: Reactive ()
  , _frpTimerMinutesEvent       :: Event (Tomato -> IO Tomato)
  , _frpTimerMinutesCb          :: Double -> Reactive ()
  , _frpSettingsPomodoroEvent   :: Event (Tomato -> IO Tomato)
  , _frpSettingsPomodoroCb      :: Double -> Reactive ()
  , _frpSettingsShortBreakEvent :: Event (Tomato -> IO Tomato)
  , _frpSettingsShortBreakCb    :: Double -> Reactive ()
  , _frpSettingsLongBreakEvent  :: Event (Tomato -> IO Tomato)
  , _frpSettingsLongBreakCb     :: Double -> Reactive ()
  , _frpSettingsIterationsEvent :: Event (Tomato -> IO Tomato)
  , _frpSettingsIterationsCb    :: Double -> Reactive ()
  , _frpSettingsVolumeEvent     :: Event (Tomato -> IO Tomato)
  , _frpSettingsVolumeCb        :: Double -> Reactive () }


data Audio = Audio
  { _audioTickTock :: S.Music
  , _audioRing     :: S.Music
  , _audioVolume   :: Int }


data App = App
  { _appUi       :: Ui
  , _appFrp      :: Frp
  , _appAudio    :: Audio }


makeLenses ''UiTimer
makeLenses ''UiSettings
makeLenses ''Ui
makeLenses ''Frp
makeLenses ''Audio
makeLenses ''App


