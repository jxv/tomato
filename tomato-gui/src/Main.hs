{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Control.Concurrent
import Data.Function
import FRP.Sodium
import Control.Lens 
import Tomato
import Graphics.UI.Gtk hiding (set, on)
import qualified Graphics.UI.Gtk as G


main :: IO ()
main =
  do void initGUI
     --
     builder <- builderNew
     builderAddFromFile builder "tomato.ui"
     --
     window <- builderGetObject builder castToWindow "window"
     --
     label_timer_interval <- builderGetObject builder castToLabel  "label_timer_interval"
     label_timer_finished <- builderGetObject builder castToLabel  "label_timer_finished"
     scale_timer_minutes  <- builderGetObject builder castToScale  "scale_timer_minutes"
     button_timer_nudge   <- builderGetObject builder castToButton "button_timer_nudge"
     --
     scale_settings_volume          <- builderGetObject builder castToScale      "scale_settings_volume"
     spinbutton_settings_pomodoro   <- builderGetObject builder castToSpinButton "spinbutton_settings_pomodoro"
     spinbutton_settings_short      <- builderGetObject builder castToSpinButton "spinbutton_settings_short"
     spinbutton_settings_long       <- builderGetObject builder castToSpinButton "spinbutton_settings_long"
     spinbutton_settings_iterations <- builderGetObject builder castToSpinButton "spinbutton_settings_iterations"
     adjustment_settings_pomodoro   <- builderGetObject builder castToAdjustment "adjustment_settings_pomodoro"
     adjustment_settings_short      <- builderGetObject builder castToAdjustment "adjustment_settings_short"
     adjustment_settings_long       <- builderGetObject builder castToAdjustment "adjustment_settings_long"
     adjustment_settings_iterations <- builderGetObject builder castToAdjustment "adjustment_settings_iterations"
     adjustment_settings_volume     <- builderGetObject builder castToAdjustment "adjustment_settings_volume"
     --
     mtom <- newMVar tomatoDef
     tom <- readMVar mtom
     spinButtonSetValue spinbutton_settings_pomodoro   (fromIntegral $ tom^.group^._1)
     adjustmentSetValue adjustment_settings_pomodoro   (fromIntegral $ tom^.group^._1)
     spinButtonSetValue spinbutton_settings_short      (fromIntegral $ tom^.group^._2)
     adjustmentSetValue adjustment_settings_short      (fromIntegral $ tom^.group^._2)
     spinButtonSetValue spinbutton_settings_long       (fromIntegral $ tom^.group^._3)
     adjustmentSetValue adjustment_settings_long       (fromIntegral $ tom^.group^._3)
     spinButtonSetValue spinbutton_settings_iterations (fromIntegral $ tom^.iterations)
     adjustmentSetValue adjustment_settings_iterations (fromIntegral $ tom^.iterations)
     --
     void $ G.on window objectDestroy mainQuit
     G.set window [ windowTitle := "Tomato" , windowResizable := False ]
     --G.set (ui^.nudgeButton) [ buttonLabel := "Start" ]
     --G.set (ui^.timerScale) [ scaleDigits := 0]
     widgetShowAll window
     mainGUI


