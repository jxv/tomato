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
  { _uiWindow                       :: Window
  , _uiTimerIntervalLabel           :: Label
  , _uiTimerCompletedLabel          :: Label
  , _uiTimerMinutesScale            :: Scale
  , _uiTimerMinutesAdjustment       :: Adjustment
  , _uiTimerNudgeButton             :: Button 
  , _uiSettingsPomodoroSpinButton   :: SpinButton
  , _uiSettingsShortSpinButton      :: SpinButton
  , _uiSettingsLongSpinButton       :: SpinButton
  , _uiSettingsIterationsSpinButton :: SpinButton
  , _uiSettingsVolumeScale          :: Scale
  , _uiSettingsPomodoroAdjustment   :: Adjustment
  , _uiSettingsShortAdjustment      :: Adjustment
  , _uiSettingsLongAdjustment       :: Adjustment
  , _uiSettingsIterationsAdjustment :: Adjustment
  , _uiSettingsVolumeAdjustment     :: Adjustment
  , _UiNotifier                     :: N.Client }


data Frp = Frp
  { _frpTimerNudgeEvent         :: Event (App -> IO App)
  , _frpTimerNudgeCb            :: Reactive ()
  , _frpTimerMinutesEvent       :: Event (App -> IO App)
  , _frpTimerMinutesCb          :: Double -> Reactive ()
  , _frpSettingsPomodoroEvent   :: Event (App -> IO App)
  , _frpSettingsPomodoroCb      :: Double -> Reactive ()
  , _frpSettingsShortBreakEvent :: Event (App -> IO App)
  , _frpSettingsShortBreakCb    :: Double -> Reactive ()
  , _frpSettingsLongBreakEvent  :: Event (App -> IO App)
  , _frpSettingsLongBreakCb     :: Double -> Reactive ()
  , _frpSettingsIterationsEvent :: Event (App -> IO App) 
  , _frpSettingsIterationsCb    :: Double -> Reactive ()
  , _frpSettingsVolumeEvent     :: Event (App -> IO App)
  , _frpSettingsVolumeCb        :: Double -> Reactive () }


data AudioRes = AudioRes
  { _audioResTickTock :: S.Music
  , _audioResRing     :: S.Music }


data App = App
  { _appTomato   :: Tomato
  , _appUi       :: Ui
  , _appFrp      :: Frp
  , _appAudioRes :: AudioRes
  , _appVolume   :: Volume }


makeLenses ''Ui
makeLenses ''Frp
makeLenses ''AudioRes
makeLenses ''App


