{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}


module Tomato.Ui where


import Control.Monad
import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Data.Function
import FRP.Sodium
import Control.Lens 
import Graphics.UI.Gtk.Gdk.Gdk
import Graphics.UI.Gtk hiding (set, on, get)
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


data Frp = Frp
  { _frpTimerNudgeEvent :: Event (Tomato -> IO Tomato)
  , _frpTimerNudgeCb :: Reactive ()
  , _frpTimerMinutesEvent :: Event (Tomato -> IO Tomato)
  , _frpTimerMinutesCb :: Reactive () }
 


makeLenses ''UiTimer
makeLenses ''UiSettings
makeLenses ''Ui
makeLenses ''Frp


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


initFrp :: IO Frp
initFrp =
  do (timer_nudge_event, timer_nudge_cb) <- sync newEvent
     (timer_minutes_event, timer_minutes_cb) <- sync newEvent
     return $ Frp
       { _frpTimerNudgeEvent = timer_nudge_event
       , _frpTimerNudgeCb = timer_nudge_cb nudgeTomatoTimer
       , _frpTimerMinutesEvent = timer_minutes_event
       , _frpTimerMinutesCb = timer_minutes_cb return }
    

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
     G.set (ut^.uiTimerMinutesAdjustment) [ adjustmentValue := (tomatoSeconds tom * secs_per_min)
                                          , adjustmentUpper := (tomatoTimeLimit tom)]
 where secs_per_min = 1 / 60


syncUiSettings :: UiSettings -> Tomato -> IO ()
syncUiSettings us tom =
  do let attrs v = [ spinButtonValue := v
                   , spinButtonDigits := 0 ]
     G.set (us^.uiSettingsPomodoroSpinButton)   (attrs $ fromIntegral (tom^.pomodoro))
     G.set (us^.uiSettingsShortSpinButton)      (attrs $ fromIntegral (tom^.shortBreak))
     G.set (us^.uiSettingsLongSpinButton)       (attrs $ fromIntegral (tom^.longBreak))
     G.set (us^.uiSettingsIterationsSpinButton) (attrs $ fromIntegral (tom^.iterations))
     G.spinButtonSetRange (us^.uiSettingsPomodoroSpinButton)   min_int_mins  max_int_mins
     G.spinButtonSetRange (us^.uiSettingsShortSpinButton)      min_int_mins  max_int_mins
     G.spinButtonSetRange (us^.uiSettingsLongSpinButton)       min_int_mins  max_int_mins
     G.spinButtonSetRange (us^.uiSettingsIterationsSpinButton) min_iter_mins max_iter_mins
     G.set (us^.uiSettingsVolumeAdjustment) [ adjustmentValue := 100
                                            , adjustmentUpper := 100 ]
 where min_int_mins = 0
       max_int_mins = 120
       min_iter_mins = 0
       max_iter_mins = 20


main :: IO ()
main =
  do void initGUI
     --
     builder <- builderNew
     builderAddFromFile builder "data/tomato.ui"
     ui <- buildUi builder
     --
     mtom <- newMVar (set pomodoro 1 tomatoDef)
     tom <- readMVar mtom
     void $ G.on (ui^.uiWindow) objectDestroy mainQuit
     G.set (ui^.uiWindow) [ windowTitle := "Tomato", windowResizable := False ]
     -- G.set (ui^.nudgeButton) [ buttonLabel := "Start" ]
     G.set (ui^.uiTimer^.uiTimerMinutesScale) [ scaleDigits := 0]
     --
     frp <- initFrp
     --
     let adj = ui^.uiTimer^.uiTimerMinutesAdjustment
     void $ onValueChanged adj (do v <- G.get adj adjustmentValue
                                   print v
                                   sync (frp^.frpTimerMinutesCb))
     void $ G.on (ui^.uiTimer^.uiTimerNudgeButton)
                 buttonPressEvent
                 (tryEvent . io $ sync (frp^.frpTimerNudgeCb))
     --
     --                                   modifyMVar_ mtom (\tom -> do let sess = set timer (Paused v) (tom^.session)
     --                                                                return (set session sess tom)))
     killFRP <- sync $ listen (merge (frp^.frpTimerMinutesEvent) (frp^.frpTimerNudgeEvent)) (modifyMVar_ mtom)
     --    
     syncUi ui tom
     widgetShowAll (ui^.uiWindow)
     --
     void $ idleAdd (do stepper ui mtom
                        return True)
                    priorityDefaultIdle
     mainGUI
     killFRP


updateMVar :: MVar a -> (a -> IO a) -> IO a
updateMVar m f = modifyMVar m (\x -> f x >>= (return . (id &&& id)))


stepper :: Ui -> MVar Tomato -> IO ()
stepper ui mtom =
  do tom <- updateMVar mtom stepTomato
     syncUi ui tom
                                  


