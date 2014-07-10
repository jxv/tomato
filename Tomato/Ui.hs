{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Tomato.Ui where

import Control.Monad
import Control.Monad.State
import Control.Applicative
import Control.Concurrent
import Data.Time.Clock
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
  <*> builderGetObject builder castToCheckButton "checkbutton_settings_five_minutes"
  <*> builderGetObject builder castToCheckButton "checkbutton_settings_final_minute"
  <*> N.connectSession
  <*> pure Nothing

initFrp :: IO Frp
initFrp =
  do (timer_nudge_event, timer_nudge_cb)                     <- sync newEvent
     (timer_minutes_event, timer_minutes_cb)                 <- sync newEvent
     (settings_pomodoro_event, settings_pomodoro_cb)         <- sync newEvent
     (settings_short_break_event, settings_short_break_cb)   <- sync newEvent
     (settings_long_break_event, settings_long_break_cb)     <- sync newEvent
     (settings_iterations_event, settings_iterations_cb)     <- sync newEvent
     (settings_volume_event, settings_volume_cb)             <- sync newEvent
     (settings_final_minute_event, settings_final_minute_cb) <- sync newEvent
     (settings_five_minutes_event, settings_five_minutes_cb) <- sync newEvent
     return $ Frp
       { _timerNudgeEvent          = timer_nudge_event
       , _timerNudgeCb             = timer_nudge_cb nudgeTimer
       , _timerMinutesEvent        = timer_minutes_event
       , _timerMinutesCb           = timer_minutes_cb . adjustTomatoTime
       , _settingsPomodoroEvent    = settings_pomodoro_event
       , _settingsPomodoroCb       = settings_pomodoro_cb . (adjustSettings pomodoro Minutes Pomodoro)
       , _settingsShortBreakEvent  = settings_short_break_event
       , _settingsShortBreakCb     = settings_short_break_cb . (adjustSettings shortBreak Minutes ShortBreak)
       , _settingsLongBreakEvent   = settings_long_break_event
       , _settingsLongBreakCb      = settings_long_break_cb . (adjustSettings longBreak Minutes LongBreak)
       , _settingsIterationsEvent  = settings_iterations_event
       , _settingsIterationsCb     = settings_iterations_cb . adjustSettingsIterations
       , _settingsVolumeEvent      = settings_volume_event
       , _settingsVolumeCb         = settings_volume_cb . adjustSettingsVolume
       , _settingsFiveMinutesEvent = settings_five_minutes_event
       , _settingsFiveMinutesCb    = settings_five_minutes_cb . adjustFiveMinutes
       , _settingsFinalMinuteEvent = settings_final_minute_event
       , _settingsFinalMinuteCb    = settings_final_minute_cb . adjustFinalMinute }

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
  <*> pure True
  <*> pure True

--

intervalName :: Interval -> String
intervalName = \case
  Pomodoro   -> "Pomodoro"
  LongBreak  -> "Long break"
  ShortBreak -> "Short break"

nudgerName :: Nudger -> String
nudgerName = show

syncUi :: App -> IO ()
syncUi app =
  do syncUiTimer    (app^.ui) (app^.tomato)
     syncUiSettings (app^.ui) (app^.tomato)

syncUiTimer :: Ui -> Tomato -> IO ()
syncUiTimer u tom =
  do G.set (u^.timerIntervalLabel)     [ labelText := intervalName (tom^.interval) ]
     G.set (u^.timerCompletedLabel)    [ labelText := ("Completed " ++ (show $ tom^.completed)) ]
     G.set (u^.timerNudgeButton)       [ buttonLabel := nudgerName (nudger tom) ]
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
           , app^.frp^.settingsVolumeEvent
           , app^.frp^.settingsFiveMinutesEvent
           , app^.frp^.settingsFinalMinuteEvent ]
     --
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
     
     void $ G.on (app^.ui^.settingsFinalMinuteCheckButton)
                 buttonPressEvent
                 (tryEvent . io $ do checked <- toggleButtonGetMode (app^.ui^.settingsFinalMinuteCheckButton)
                                     sync $ (app^.frp^.settingsFinalMinuteCb) (not checked))

     void $ G.on (app^.ui^.settingsFiveMinutesCheckButton)
                 buttonPressEvent
                 (tryEvent . io $ do checked <- toggleButtonGetMode (app^.ui^.settingsFiveMinutesCheckButton)
                                     sync $ (app^.frp^.settingsFiveMinutesCb) (not checked))

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
     --
     mainGUI
     S.closeAudio
     S.quit
     killFRP

--

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
     return app

adjustSettings f g int n app =
  let tom' = set f (g n) (app^.tomato)
      app' = set tomato tom' app
  in return app'

adjustFinalMinute :: Bool -> App -> IO App
adjustFinalMinute checked app =
  do toggleButtonSetMode (app^.ui^.settingsFinalMinuteCheckButton) checked
     return $ set finalMinute checked app

adjustFiveMinutes :: Bool -> App -> IO App
adjustFiveMinutes checked app =
  do toggleButtonSetMode (app^.ui^.settingsFiveMinutesCheckButton) checked
     return $ set fiveMinutes checked app

nudgeTimer :: App -> IO App
nudgeTimer app =
  do cur_time <- getCurrentTime
     let tom' = nudgeTomatoTimer (app^.tomato) cur_time
         (t0,t1) = (app^.tomato^.timer, tom'^.timer)
     when (stopTickTock t0 t1 || stopRing t0 t1) $ S.pauseMusic
     when (startTickTock t0 t1) $ S.playMusic (app^.audioRes^.tickTockMusic) (-1)
     return (set tomato tom' app)

--

stepper :: MVar App -> IO ()
stepper mapp =
  do modifyMVar_ mapp $ \app ->
       do cur_time <- getCurrentTime
          let tom = stepTomato (app^.tomato) cur_time
          when (startRing (app^.tomato^.timer) (tom^.timer))
               (S.playMusic (app^.audioRes^.ringMusic) 0)
          syncUiTimer (app^.ui) tom
          --
          let secs  = tomatoSeconds (app^.tomato)
              secs' = tomatoSeconds tom
              time_limit = tomatoTimeLimit tom
              int_name = intervalName (tom^.interval)
              cln = app^.ui^.notifierClient
              mins_left = round time_limit - (floor $ toMinutes secs')
          --
          let final_min_left = app^.finalMinute && startingLastMinute time_limit secs secs'
              five_mins_left = app^.fiveMinutes && startingEveryNthMinutes 5 secs secs' && not is_finished
              is_finished = app^.tomato^.timer /= Finished && tom^.timer == Finished
              was_finished = app^.tomato^.timer == Finished && tom^.timer /= Finished
          --
          when final_min_left (notify_ cln $ easyNote int_name "1 minute left")
          when five_mins_left (notify_ cln $ easyNote int_name (show mins_left ++ " minutes left"))
          when is_finished  (notify_ cln $ easyNote int_name "Finished")
          --
          when is_finished (G.set (app^.ui^.window) [windowUrgencyHint := True])
          when was_finished (G.set (app^.ui^.window) [windowUrgencyHint := False])
          --
          return $ set tomato tom app
     threadDelay 100000

--

easyNote :: String -> String -> N.Note
easyNote title content = N.blankNote { N.summary = title, N.body = Just (N.Text content) }

notify_ :: N.Client -> N.Note -> IO ()
notify_ client note = void (N.notify client note)

--

io :: MonadIO m => IO a -> m a
io = liftIO

