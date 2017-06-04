import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig

main = do
  spawn "xmobar"

  xmonad $ desktopConfig
      { borderWidth = 2
      , focusFollowsMouse = False
      , focusedBorderColor = colorFocusedBorder
      , layoutHook = smartBorders $ layoutHook desktopConfig
      , normalBorderColor = colorNormalBorder
      , modMask = mod4Mask
      , terminal = "konsole"
      }
      `additionalKeysP`
        [ ("M-e", spawn "emacs")
        , ("M-c", spawn "google-chrome-stable")
        , ("M-p", spawn "dmenu_run -fn 'monospace-18' -b")
        , ("M-q", spawn "killall xmobar; xmonad --recompile && xmonad --restart")
        ]

colorNormalBorder = "#CCCCC6"
colorFocusedBorder = "#FD971F"
