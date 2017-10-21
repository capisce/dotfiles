import System.IO

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)

main = do
  xmproc <- spawnPipe "xmobar"

  spawn "stalonetray"

  xmonad $ desktopConfig
      { borderWidth = 2
      , focusFollowsMouse = False
      , focusedBorderColor = colorFocusedBorder
      , layoutHook = smartBorders $ layoutHook desktopConfig
      , logHook = dynamicLogWithPP defaultPP
                    { ppCurrent = xmobarColor "gray" "" . wrap "<" ">"
                    , ppHidden = xmobarColor "gray" ""
                    , ppHiddenNoWindows = const ""
                    , ppLayout = const ""
                    , ppOutput = hPutStrLn xmproc
                    , ppSep = " <fc=#848484>|</fc> "
                    , ppTitle = xmobarColor "gray" "" . shorten 40
                    , ppWsSep = " "
                    }
      , manageHook = myManageHook <+> manageHook desktopConfig
      , modMask = mod4Mask
      , normalBorderColor = colorNormalBorder
      , terminal = "konsole"
      }
      `additionalKeysP`
        [ ("M-e", spawn "emacsclient -c")
        , ("M-c", spawn "emacsclient -c -F \"((name . \\\"emacs-capture\\\"))\" -e \"(org-capture)\"")
        , ("M-b", spawn "google-chrome-stable")
        , ("M-p", spawn "dmenu_run -fn 'monospace-14' -b")
        , ("M-q", spawn $ "killall xmobar; killall stalonetray;"
                       ++ "xmonad --recompile && xmonad --restart")
        , ("M-S-z", spawn "xscreensaver-command -lock")
        , ("<XF86AudioMute>", spawn "amixer set Master toggle")
        , ("<XF86AudioLowerVolume>", spawn "amixer set Master 2%-")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 2%+")
        ]

colorNormalBorder = "#CCCCC6"
colorFocusedBorder = "#FD971F"

myManageHook = appName =? "emacs-capture" --> doRectFloat (W.RationalRect 0.2 0.25 0.6 0.5)
