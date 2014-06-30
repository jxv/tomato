{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}


module Tomato.Ui where


import Control.Monad
import Control.Applicative
import Control.Concurrent
import Data.Function
import FRP.Sodium
import Control.Lens 
import Graphics.UI.Gtk hiding (set, on)
import qualified Graphics.UI.Gtk as G

import Tomato.Core


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
  , _uiSettings :: UiSettings }


makeLenses ''UiTimer
makeLenses ''UiSettings
makeLenses ''Ui


--


buildUiTimer :: Builder -> IO UiTimer
buildUiTimer builder =
  do label_timer_interval     <- builderGetObject builder castToLabel      "label_timer_interval"
     label_timer_completed    <- builderGetObject builder castToLabel      "label_timer_completed"
     scale_timer_minutes      <- builderGetObject builder castToScale      "scale_timer_minutes"
     adjustment_timer_minutes <- builderGetObject builder castToAdjustment "adjustment_timer_minutes"
     button_timer_nudge       <- builderGetObject builder castToButton     "button_timer_nudge"
     return $ UiTimer
       { _uiTimerIntervalLabel     = label_timer_interval
       , _uiTimerCompletedLabel    = label_timer_completed
       , _uiTimerMinutesScale      = scale_timer_minutes
       , _uiTimerMinutesAdjustment = adjustment_timer_minutes
       , _uiTimerNudgeButton       = button_timer_nudge }


buildUiSettings :: Builder -> IO UiSettings
buildUiSettings builder =
  do spinbutton_settings_pomodoro   <- builderGetObject builder castToSpinButton "spinbutton_settings_pomodoro"
     spinbutton_settings_short      <- builderGetObject builder castToSpinButton "spinbutton_settings_short"
     spinbutton_settings_long       <- builderGetObject builder castToSpinButton "spinbutton_settings_long"
     spinbutton_settings_iterations <- builderGetObject builder castToSpinButton "spinbutton_settings_iterations"
     scale_settings_volume          <- builderGetObject builder castToScale      "scale_settings_volume"
     adjustment_settings_pomodoro   <- builderGetObject builder castToAdjustment "adjustment_settings_pomodoro"
     adjustment_settings_short      <- builderGetObject builder castToAdjustment "adjustment_settings_short"
     adjustment_settings_long       <- builderGetObject builder castToAdjustment "adjustment_settings_long"
     adjustment_settings_iterations <- builderGetObject builder castToAdjustment "adjustment_settings_iterations"
     adjustment_settings_volume     <- builderGetObject builder castToAdjustment "adjustment_settings_volume"
     return $ UiSettings
       { _uiSettingsPomodoroSpinButton   = spinbutton_settings_pomodoro
       , _uiSettingsShortSpinButton      = spinbutton_settings_short
       , _uiSettingsLongSpinButton       = spinbutton_settings_long
       , _uiSettingsIterationsSpinButton = spinbutton_settings_iterations
       , _uiSettingsVolumeScale          = scale_settings_volume
       , _uiSettingsPomodoroAdjustment   = adjustment_settings_pomodoro
       , _uiSettingsShortAdjustment      = adjustment_settings_short
       , _uiSettingsLongAdjustment       = adjustment_settings_long
       , _uiSettingsIterationsAdjustment = adjustment_settings_iterations
       , _uiSettingsVolumeAdjustment     = adjustment_settings_volume }


buildUi :: Builder -> IO Ui
buildUi builder = Ui
  <$> (builderGetObject builder castToWindow "window")
  <*> (buildUiTimer builder)
  <*> (buildUiSettings builder)


--


syncUi :: Ui -> Tomato -> IO ()
syncUi ui tom =
  do syncUiTimer (ui^.uiTimer) tom
     syncUiSettings (ui^.uiSettings) tom


syncUiTimer :: UiTimer -> Tomato -> IO ()
syncUiTimer ut tom =
  do G.set (ut^.uiTimerIntervalLabel)     [ labelText := (show $ tom^.session^.interval) ]
     G.set (ut^.uiTimerCompletedLabel)    [ labelText := ("Completed " ++ (show $ tom^.completed)) ]
     G.set (ut^.uiTimerNudgeButton)       [ buttonLabel := (show $ nudger tom) ]
     G.set (ut^.uiTimerMinutesAdjustment) [ adjustmentValue := (tomatoSeconds tom / 60)
                                          , adjustmentUpper := (tomatoTimeLimit tom)]


syncUiSettings :: UiSettings -> Tomato -> IO ()
syncUiSettings us tom =
  do let attrs v = [ spinButtonValue := v
                   , spinButtonDigits := 0 ]
     G.set (us^.uiSettingsPomodoroSpinButton)   (attrs $ fromIntegral (tom^.pomodoro))
     G.set (us^.uiSettingsShortSpinButton)      (attrs $ fromIntegral (tom^.shortBreak))
     G.set (us^.uiSettingsLongSpinButton)       (attrs $ fromIntegral (tom^.longBreak))
     G.set (us^.uiSettingsIterationsSpinButton) (attrs $ fromIntegral (tom^.iterations))
     G.spinButtonSetRange (us^.uiSettingsPomodoroSpinButton)   0 120
     G.spinButtonSetRange (us^.uiSettingsShortSpinButton)      0 120
     G.spinButtonSetRange (us^.uiSettingsLongSpinButton)       0 120
     G.spinButtonSetRange (us^.uiSettingsIterationsSpinButton) 0 20
     G.set (us^.uiSettingsVolumeAdjustment) [ adjustmentValue := 100
                                            , adjustmentUpper := 100 ]


main :: IO ()
main =
  do void initGUI
     --
     builder <- builderNew
     builderAddFromFile builder "data/tomato.ui"
     ui <- buildUi builder
     --
     mtom <- newMVar tomatoDef
     tom <- readMVar mtom
     void $ G.on (ui^.uiWindow) objectDestroy mainQuit
     G.set (ui^.uiWindow) [ windowTitle := "Tomato", windowResizable := False ]
     -- G.set (ui^.nudgeButton) [ buttonLabel := "Start" ]
     G.set (ui^.uiTimer^.uiTimerMinutesScale) [ scaleDigits := 0]
     syncUi ui tom
     widgetShowAll (ui^.uiWindow)
     mainGUI


