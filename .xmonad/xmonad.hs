import System.IO

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Layout.NoBorders
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
      , normalBorderColor = colorNormalBorder
      , modMask = mod4Mask
      , terminal = "konsole"
      }
      `additionalKeysP`
        [ ("M-e", spawn "emacs")
        , ("M-c", spawn "google-chrome-stable")
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
