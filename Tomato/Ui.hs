{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}


module Tomato.Ui where


import Control.Monad
import Control.Monad.State
import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Data.Function
import FRP.Sodium
import Control.Lens 
import qualified DBus.Notify as N
import qualified Graphics.UI.SDL       as S
import qualified Graphics.UI.SDL.Mixer as S
import Graphics.UI.Gtk.Gdk.Gdk
import Graphics.UI.Gtk hiding (set, on, get)
import qualified Graphics.UI.Gtk as G

import Paths_tomato
import Tomato.Core
import Tomato.Ui.Types


--


buildUi :: Builder -> IO Ui
buildUi builder = Ui
  <$> builderGetObject builder castToWindow      "window"
  <*> builderGetObject builder castToLabel       "label_timer_interval"
  <*> builderGetObject builder castToLabel       "label_timer_completed"
  <*> builderGetObject builder castToScale       "scale_timer_minutes"
  <*> builderGetObject builder castToAdjustment  "adjustment_timer_minutes"
  <*> builderGetObject builder castToButton      "button_timer_nudge"
  <*> builderGetObject builder castToSpinButton  "spinbutton_settings_pomodoro"
  <*> builderGetObject builder castToSpinButton  "spinbutton_settings_short"
  <*> builderGetObject builder castToSpinButton  "spinbutton_settings_long"
  <*> builderGetObject builder castToSpinButton  "spinbutton_settings_iterations"
  <*> builderGetObject builder castToScale       "scale_settings_volume"
  <*> builderGetObject builder castToAdjustment  "adjustment_settings_pomodoro"
  <*> builderGetObject builder castToAdjustment  "adjustment_settings_short"
  <*> builderGetObject builder castToAdjustment  "adjustment_settings_long"
  <*> builderGetObject builder castToAdjustment  "adjustment_settings_iterations"
  <*> builderGetObject builder castToAdjustment  "adjustment_settings_volume"
  <*> builderGetObject builder castToCheckButton "checkbutton_settings_final_minute"
  <*> N.connectSession


initFrp :: IO Frp
initFrp =
  do (timer_nudge_event, timer_nudge_cb)                   <- sync newEvent
     (timer_minutes_event, timer_minutes_cb)               <- sync newEvent
     (settings_pomodoro_event, settings_pomodoro_cb)       <- sync newEvent
     (settings_short_break_event, settings_short_break_cb) <- sync newEvent
     (settings_long_break_event, settings_long_break_cb)   <- sync newEvent
     (settings_iterations_event, settings_iterations_cb)   <- sync newEvent
     (settings_volume_event, settings_volume_cb)           <- sync newEvent
     return $ Frp
       { _timerNudgeEvent         = timer_nudge_event
       , _timerNudgeCb            = timer_nudge_cb nudgeTimer
       , _timerMinutesEvent       = timer_minutes_event
       , _timerMinutesCb          = timer_minutes_cb . adjustTomatoTime
       , _settingsPomodoroEvent   = settings_pomodoro_event
       , _settingsPomodoroCb      = settings_pomodoro_cb . (adjustSettings pomodoro Minutes Pomodoro)
       , _settingsShortBreakEvent = settings_short_break_event
       , _settingsShortBreakCb    = settings_short_break_cb . (adjustSettings shortBreak Minutes ShortBreak)
       , _settingsLongBreakEvent  = settings_long_break_event
       , _settingsLongBreakCb     = settings_long_break_cb . (adjustSettings longBreak Minutes LongBreak)
       , _settingsIterationsEvent = settings_iterations_event
       , _settingsIterationsCb    = settings_iterations_cb . adjustSettingsIterations
       , _settingsVolumeEvent     = settings_volume_event
       , _settingsVolumeCb        = settings_volume_cb . adjustSettingsVolume }


initAudioRes :: IO AudioRes
initAudioRes = AudioRes
  <$> (S.loadMUS =<< getDataFileName "tick_tock.ogg")
  <*> (S.loadMUS =<< getDataFileName "ring.ogg")


initApp :: IO App
initApp = App
  <$> pure tomatoDef
  <*> (do builder <- builderNew
          builderAddFromFile builder =<< getDataFileName "tomato.ui"
          buildUi builder)
  <*> initFrp
  <*> initAudioRes
  <*> pure 100


--


intervalName :: Interval -> String
intervalName = \case
  Pomodoro   -> "Pomodoro"
  LongBreak  -> "Long break"
  ShortBreak -> "Short break"


syncUi :: App -> IO ()
syncUi app =
  do syncUiTimer    (app^.ui) (app^.tomato)
     syncUiSettings (app^.ui) (app^.tomato)


syncUiTimer :: Ui -> Tomato -> IO ()
syncUiTimer u tom =
  do G.set (u^.timerIntervalLabel)     [ labelText := intervalName (tom^.interval) ]
     G.set (u^.timerCompletedLabel)    [ labelText := ("Completed " ++ (show $ tom^.completed)) ]
     G.set (u^.timerNudgeButton)       [ buttonLabel := (show $ nudger tom) ]
     G.set (u^.timerMinutesAdjustment) [ adjustmentValue := (minutes . toMinutes $ tomatoSeconds tom)
                                          , adjustmentUpper := (minutes $ tomatoTimeLimit tom)]
 where secs_per_min = 1 / 60


syncUiSettings :: Ui -> Tomato -> IO ()
syncUiSettings u tom =
  do let attrs v = [ spinButtonValue := v
                   , spinButtonDigits := 0 ]
     G.set (u^.settingsPomodoroSpinButton)   (attrs $ minutes (tom^.pomodoro))
     G.set (u^.settingsShortSpinButton)      (attrs $ minutes (tom^.shortBreak))
     G.set (u^.settingsLongSpinButton)       (attrs $ minutes (tom^.longBreak))
     G.set (u^.settingsIterationsSpinButton) (attrs $ fromIntegral (tom^.iterations))
     G.spinButtonSetRange (u^.settingsPomodoroSpinButton)   min_int_mins  max_int_mins
     G.spinButtonSetRange (u^.settingsShortSpinButton)      min_int_mins  max_int_mins
     G.spinButtonSetRange (u^.settingsLongSpinButton)       min_int_mins  max_int_mins
     G.spinButtonSetRange (u^.settingsIterationsSpinButton) min_iter_mins max_iter_mins
     G.set (u^.settingsVolumeAdjustment) [ adjustmentUpper := 100 ]
 where min_int_mins = 0
       max_int_mins = 120
       min_iter_mins = 0
       max_iter_mins = 20


main :: IO ()
main =
  do void initGUI
     S.init [S.InitAudio]
     S.openAudio 22050 S.AudioS16Sys 2 4096
     S.setMusicVolume 100
     --
     app <- initApp
     --
     -- let start_note = N.blankNote { N.summary = "Starting", N.body = Just $ N.Text "test" }
     -- void $ N.notify client start_note
     --
     mapp <- newMVar app
     void $ G.on (app^.ui^.window) objectDestroy mainQuit
     G.set (app^.ui^.window) [ windowTitle := "Tomato", windowResizable := False ]
     G.set (app^.ui^.timerMinutesScale) [ scaleDigits := 0]
     --
     let evts =
           [ app^.frp^.timerMinutesEvent
           , app^.frp^.timerNudgeEvent
           , app^.frp^.settingsPomodoroEvent 
           , app^.frp^.settingsShortBreakEvent 
           , app^.frp^.settingsLongBreakEvent 
           , app^.frp^.settingsIterationsEvent
           , app^.frp^.settingsVolumeEvent ]
     killFRP <- sync $ listen (foldr1 merge evts) (modifyMVar_ mapp)
     --
     void $ G.on (app^.ui^.timerMinutesScale)
                 changeValue
                 (\_ v -> do sync $ (app^.frp^.timerMinutesCb) v
                             return True)

     void $ G.on (app^.ui^.settingsVolumeScale)
                 changeValue
                 (\_ v -> do sync $ (app^.frp^.settingsVolumeCb) v
                             G.set (app^.ui^.settingsVolumeAdjustment) [ adjustmentValue := v ]
                             return True)
     
     void $ G.on (app^.ui^.timerNudgeButton)
                 buttonPressEvent
                 (tryEvent . io $ sync (app^.frp^.timerNudgeCb))

     let spinButtonCb sb cb = onOutput (app^.ui^.sb) $
           do v <- spinButtonGetValue (app^.ui^.sb)
              sync $ (app^.frp^.cb) v
              return False
     --
     spinButtonCb settingsPomodoroSpinButton   settingsPomodoroCb
     spinButtonCb settingsShortSpinButton      settingsShortBreakCb
     spinButtonCb settingsLongSpinButton       settingsLongBreakCb
     spinButtonCb settingsIterationsSpinButton settingsIterationsCb
     --
     syncUi app
     G.set (app^.ui^.settingsVolumeAdjustment) [ adjustmentValue := 100 ]
     widgetShowAll (app^.ui^.window)
     --
     void $ idleAdd (do stepper mapp
                        return True)
                    priorityDefaultIdle
     mainGUI
     S.closeAudio
     S.quit
     killFRP


adjustTomatoTime :: Double -> App -> IO App
adjustTomatoTime mins app =
  do S.pauseMusic
     let tmr = Paused $ limitSecondsForTimerByMinutes (app^.tomato) (Minutes mins)
         tom = set timer tmr (app^.tomato)
     return $ set tomato tom app


adjustSettingsIterations :: Double -> App -> IO App
adjustSettingsIterations iter app = return $ app
  { _tomato = set iterations (round iter) (app^.tomato) }


adjustSettingsVolume :: Double -> App -> IO App
adjustSettingsVolume n app =
  do S.setMusicVolume (round n)
     -- G.set (ui^.settingsVolumeAdjustment) [ adjustmentValue := 100 ]
     return app


-- adjustSettings ::  ((Double -> Identity Double) -> Tomato -> Identity Tomato) -> Interval ->
--                   Double -> Tomato -> IO Tomato
adjustSettings f g int n app =
  let tom' = set f (g n) (app^.tomato)
      app' = set tomato tom' app
  in if (tom'^.interval) /= int
        then return app'
        else do S.pauseMusic
                return $ case (tom'^.timer) of
                  Running s _ -> app' { _tomato = set timer (Paused s) tom' }
                  _           -> app'


updateMVar :: MVar a -> (a -> IO a) -> IO a
updateMVar m f = modifyMVar m (\x -> f x >>= (return . (id &&& id)))


stepper :: MVar App -> IO ()
stepper mapp =
  do modifyMVar_ mapp $ \app ->
       do tom <- stepTomato (app^.tomato)
          when (startRing (app^.tomato^.timer) (tom^.timer))
               (S.playMusic (app^.audioRes^.ringMusic) 0)
          syncUiTimer (app^.ui) tom
          -- 
          -- last minute notification
          when (startingLastMinute (tomatoTimeLimit tom) (tomatoSeconds $ app^.tomato) (tomatoSeconds tom)) $
               void $ N.notify (app^.ui^.notifierClient)
                               (N.blankNote { N.summary = (intervalName $ tom^.interval) 
                                            , N.body = Just $ N.Text "1 minute left" })
          --
          return $ set tomato tom app
     threadDelay 100000


io :: MonadIO m => IO a -> m a
io = liftIO


--


nudgeTimer :: App -> IO App
nudgeTimer app =
  do tom' <- nudgeTomatoTimer (app^.tomato)
     let (t0,t1) = (app^.tomato^.timer, tom'^.timer)
     when (stopTickTock t0 t1 || stopRing t0 t1) $ S.pauseMusic
     when (startTickTock t0 t1) $ S.playMusic (app^.audioRes^.tickTockMusic) (-1)
     return (set tomato tom' app)


