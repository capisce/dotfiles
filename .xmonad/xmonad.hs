import XMonad
import XMonad.Util.EZConfig

main = xmonad $ defaultConfig
  { modMask = mod4Mask
  , terminal = "konsole"
  }
  `additionalKeysP`
  [ ("M-e", spawn "emacs")
  , ("M-b", spawn "google-chrome-stable")
  ]
