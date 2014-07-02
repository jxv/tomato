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
import qualified Graphics.UI.SDL       as S
import qualified Graphics.UI.SDL.Mixer as S
import Graphics.UI.Gtk.Gdk.Gdk
import Graphics.UI.Gtk hiding (set, on, get)
import qualified Graphics.UI.Gtk as G

import Paths_tomato
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
  , _frpSettingsIterationsCb    :: Double -> Reactive () }


data Audio = Audio
  { _audioTickTock :: S.Music
  , _audioRing     :: S.Music }


makeLenses ''UiTimer
makeLenses ''UiSettings
makeLenses ''Ui
makeLenses ''Frp
makeLenses ''Audio


--


buildUiTimer :: Builder -> IO UiTimer
buildUiTimer builder = UiTimer
  <$> builderGetObject builder castToLabel      "label_timer_interval"
  <*> builderGetObject builder castToLabel      "label_timer_completed"
  <*> builderGetObject builder castToScale      "scale_timer_minutes"
  <*> builderGetObject builder castToAdjustment "adjustment_timer_minutes"
  <*> builderGetObject builder castToButton     "button_timer_nudge"


buildUiSettings :: Builder -> IO UiSettings
buildUiSettings builder = UiSettings
  <$> builderGetObject builder castToSpinButton "spinbutton_settings_pomodoro"
  <*> builderGetObject builder castToSpinButton "spinbutton_settings_short"
  <*> builderGetObject builder castToSpinButton "spinbutton_settings_long"
  <*> builderGetObject builder castToSpinButton "spinbutton_settings_iterations"
  <*> builderGetObject builder castToScale      "scale_settings_volume"
  <*> builderGetObject builder castToAdjustment "adjustment_settings_pomodoro"
  <*> builderGetObject builder castToAdjustment "adjustment_settings_short"
  <*> builderGetObject builder castToAdjustment "adjustment_settings_long"
  <*> builderGetObject builder castToAdjustment "adjustment_settings_iterations"
  <*> builderGetObject builder castToAdjustment "adjustment_settings_volume"


buildUi :: Builder -> IO Ui
buildUi builder = Ui
  <$> builderGetObject builder castToWindow "window"
  <*> buildUiTimer builder
  <*> buildUiSettings builder


initFrp :: Audio -> IO Frp
initFrp ad =
  do (timer_nudge_event, timer_nudge_cb)                   <- sync newEvent
     (timer_minutes_event, timer_minutes_cb)               <- sync newEvent
     (settings_pomodoro_event, settings_pomodoro_cb)       <- sync newEvent
     (settings_short_break_event, settings_short_break_cb) <- sync newEvent
     (settings_long_break_event, settings_long_break_cb)   <- sync newEvent
     (settings_iterations_event, settings_iterations_cb)   <- sync newEvent
     return $ Frp
       { _frpTimerNudgeEvent         = timer_nudge_event
       , _frpTimerNudgeCb            = timer_nudge_cb (\tom -> do tom' <- nudgeTomatoTimer tom
                                                                  let (t0,t1) = (tom^.session^.timer, tom'^.session^.timer)
                                                                  when (stopTickTock t0 t1 || stopRing t0 t1) $ S.pauseMusic
                                                                  when (startTickTock t0 t1) $ S.playMusic (ad^.audioTickTock) (-1)
                                                                  return tom')
       , _frpTimerMinutesEvent       = timer_minutes_event
       , _frpTimerMinutesCb          = timer_minutes_cb . (adjustTomatoTime ad)
       , _frpSettingsPomodoroEvent   = settings_pomodoro_event
       , _frpSettingsPomodoroCb      = settings_pomodoro_cb . (adjustSettings pomodoro Pomodoro)
       , _frpSettingsShortBreakEvent = settings_short_break_event
       , _frpSettingsShortBreakCb    = settings_short_break_cb . (adjustSettings shortBreak ShortBreak)
       , _frpSettingsLongBreakEvent  = settings_long_break_event
       , _frpSettingsLongBreakCb     = settings_long_break_cb . (adjustSettings longBreak LongBreak)
       , _frpSettingsIterationsEvent = settings_iterations_event
       , _frpSettingsIterationsCb    = settings_iterations_cb . adjustSettingsIterations }


initAudio :: IO Audio
initAudio =
  do tick_tock <- S.loadMUS =<< getDataFileName "data/tick_tock.ogg"
     ring      <- S.loadMUS =<< getDataFileName "data/ring.ogg"
     return $ Audio
       { _audioTickTock = tick_tock
       , _audioRing     = ring }


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
     S.init [S.InitAudio]
     S.openAudio 22050 S.AudioS16Sys 2 4096
     S.setMusicVolume 100
     audio <- initAudio
     S.setMusicVolume 100
     --
     builder <- builderNew
     builderAddFromFile builder =<< getDataFileName "data/tomato.ui"
     ui <- buildUi builder
     --
     mtom <- newMVar tomatoDef
     tom <- readMVar mtom
     void $ G.on (ui^.uiWindow) objectDestroy mainQuit
     G.set (ui^.uiWindow) [ windowTitle := "Tomato", windowResizable := False ]
     G.set (ui^.uiTimer^.uiTimerMinutesScale) [ scaleDigits := 0]
     --
     frp <- initFrp audio
     let evts =
           [ frp^.frpTimerMinutesEvent
           , frp^.frpTimerNudgeEvent
           , frp^.frpSettingsPomodoroEvent 
           , frp^.frpSettingsShortBreakEvent 
           , frp^.frpSettingsLongBreakEvent 
           , frp^.frpSettingsIterationsEvent ]
     killFRP <- sync $ listen (foldr1 merge evts) (modifyMVar_ mtom)
     --
     void $ G.on (ui^.uiTimer^.uiTimerMinutesScale)
                 changeValue
                 (\_ v -> do sync $ (frp^.frpTimerMinutesCb) v
                             return True)
     
     void $ G.on (ui^.uiTimer^.uiTimerNudgeButton)
                 buttonPressEvent
                 (tryEvent . io $ sync (frp^.frpTimerNudgeCb))

     let spinButtonCb sb cb = onOutput (ui^.uiSettings^.sb) $
           do v <- spinButtonGetValue (ui^.uiSettings^.sb)
              sync $ (frp^.cb) v
              return False

     spinButtonCb uiSettingsPomodoroSpinButton   frpSettingsPomodoroCb
     spinButtonCb uiSettingsShortSpinButton      frpSettingsShortBreakCb
     spinButtonCb uiSettingsLongSpinButton       frpSettingsLongBreakCb
     spinButtonCb uiSettingsIterationsSpinButton frpSettingsIterationsCb

     --
     syncUi ui tom
     widgetShowAll (ui^.uiWindow)
     --
     void $ idleAdd (do stepper audio ui mtom
                        return True)
                    priorityDefaultIdle
     mainGUI
     S.closeAudio
     S.quit
     killFRP


adjustTomatoTime :: Audio -> Double -> Tomato -> IO Tomato
adjustTomatoTime ad new_time tom =
  do let time_in_range = limitSecondsForTimerByMinutes tom new_time
         sess = set timer (Paused time_in_range) (tom^.session)
     S.pauseMusic
     return (set session sess tom)


adjustSettings ::  ((Int -> Identity Int) -> Tomato -> Identity Tomato) -> Interval ->
                  Double -> Tomato -> IO Tomato
adjustSettings f int n tom =
  let tom' = set f (round n) tom
  in if (tom'^.session^.interval) /= int
        then return tom'
        else let sess = case (tom'^.session^.timer) of
                   Running s _ -> set timer (Paused s) (tom'^.session)
                   _           -> tom'^.session
             in return (set session sess tom')


adjustSettingsIterations :: Double -> Tomato -> IO Tomato
adjustSettingsIterations iter tom = return $ set iterations (round iter) tom


updateMVar :: MVar a -> (a -> IO a) -> IO a
updateMVar m f = modifyMVar m (\x -> f x >>= (return . (id &&& id)))


stepper :: Audio -> Ui -> MVar Tomato -> IO ()
stepper ad ui mtom =
  do tom <- updateMVar mtom (\t0-> do t1 <- stepTomato t0
                                      when (startRing (t0^.session^.timer) (t1^.session^.timer))
                                           (S.playMusic (ad^.audioRing) 0)
                                      return t1)
     syncUiTimer (ui^.uiTimer) tom
     threadDelay 100000 -- don't update too fast


