{-# LANGUAGE RecordWildCards #-}
import XMonad
import XMonad.Layout
import XMonad.Layout.ResizableTile
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers (composeOne, doCenterFloat, isDialog, (-?>))
import XMonad.Util.Run(safeSpawnProg, runInTerm)
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86( xF86XK_AudioRaiseVolume
                                   , xF86XK_AudioLowerVolume
                                   , xF86XK_AudioMute
                                   , xF86XK_MonBrightnessDown
                                   , xF86XK_MonBrightnessUp
                                   )

scrot = "scrot -e 'mv $f ~/screenshots'"
scrotMouse = "sleep 0.2; " ++ scrot ++ " -s"

-- NB: trailing slash
keybindScripts = "/home/lkirk/.xmonad/keybind_scripts/"

keyLayout = keys defaultConfig <+> \conf@(XConfig { .. })
   -> M.fromList
      [ ((0, xF86XK_AudioRaiseVolume), spawn "amixer -c PCH set Master 4+")
      , ((0, xF86XK_AudioLowerVolume), spawn "amixer -c PCH set Master 4-")
      , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")

      , ((shiftMask, xF86XK_AudioRaiseVolume), spawn "amixer -c PCH set Master 2+")
      , ((shiftMask, xF86XK_AudioLowerVolume), spawn "amixer -c PCH set Master 2-")

      , ((modMask .|. shiftMask, xK_f), spawn (keybindScripts ++ "browser"))
      -- NB: not in keybind dir
      , ((modMask .|. shiftMask, xK_h), runInTerm "-title htop" "sh -c 'htop'")

      , ((0, xF86XK_MonBrightnessDown), spawn (keybindScripts ++ "backlight-down"))
      , ((0, xF86XK_MonBrightnessUp), spawn (keybindScripts ++ "backlight-up"))

      , ((modMask .|. shiftMask, xK_l), safeSpawnProg "xsecurelock")
      , ((modMask, xK_F12), spawn scrotMouse)
      , ((modMask, xK_F11), spawn scrot) 

      , ((modMask .|. shiftMask, xK_a),  -- "Remote server access"
              safeSpawnProg (keybindScripts ++ "ssh-remote-server"))
      , ((modMask .|. shiftMask, xK_x),  -- "Remote emacs server"
              runInTerm "-title emacs-remote" (keybindScripts ++ "emacs-remote"))

      , ((modMask .|. shiftMask, xK_z),  -- "Local emacs"
              runInTerm "-title emacs" (keybindScripts ++ "emacs"))

      , ((modMask .|. shiftMask, xK_s),  -- "Sound control"
              runInTerm "-title alsamixer" (keybindScripts ++ "alsamixer"))

      -- "Turn off external monitor"
      , ((modMask .|. shiftMask, xK_n), spawn (keybindScripts ++ "monoff"))
      -- "Turn on external monitor"
      , ((modMask .|. shiftMask, xK_m), spawn (keybindScripts ++ "mon"))

      , ((modMask .|. shiftMask, xK_minus), sendMessage MirrorShrink)
      , ((modMask .|. shiftMask, xK_equal), sendMessage MirrorExpand)]

layout = ResizableTall 1 (3/100) (1/2) []
         ||| Mirror tiled
         ||| Full
    where
      -- default tiling algorithm partitions the screen into two panes
      tiled   = Tall nmaster delta ratio
      nmaster = 1     -- the default number of windows in the master pane
      ratio   = 1/2   -- default proportion of screen occupied by master pane
      delta   = 3/100 -- percent of screen to increment by when resizing panes

mhook = composeOne [ isDialog     -?> doCenterFloat ]
                    <+> composeAll [ className =? "zoom" --> doFloat ]

main = xmonad =<< xmobar defaultConfig
       {
        terminal    = "xterm"
       , borderWidth = 0
       , keys = keyLayout
       , layoutHook = layout
       , manageHook = mhook
       }
