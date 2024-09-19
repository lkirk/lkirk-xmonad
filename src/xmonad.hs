{-# LANGUAGE RecordWildCards #-}

import qualified Data.Map as M
import qualified Data.Map.Strict
import Graphics.X11.ExtraTypes.XF86
  ( xF86XK_AudioLowerVolume,
    xF86XK_AudioMute,
    xF86XK_AudioRaiseVolume,
    xF86XK_MonBrightnessDown,
    xF86XK_MonBrightnessUp,
  )
import XMonad
import qualified XMonad.Config as DefaultConfig
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (composeOne, doCenterFloat, isDialog, isNotification, (-?>))
import XMonad.Layout.CenteredIfSingle
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import qualified XMonad.StackSet as W
import XMonad.Util.Run (runInTerm, safeSpawnProg)

scrot :: [Char]
scrot = "scrot -e 'mv $f ~/screenshots'"

scrotMouse :: [Char]
scrotMouse = "sleep 0.2; " ++ scrot ++ " -s"

-- NB: trailing slash
keybindScripts :: [Char]
keybindScripts = "/home/lkirk/.xmonad/keybind_scripts/"

winMask = mod4Mask

-- keyLayout :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
keyLayout =
  keys DefaultConfig.def <+> \conf@XConfig {..} ->
    M.fromList
      -- [ ((0, xF86XK_AudioRaiseVolume), spawn "amixer -c PCH set Master 4+")
      -- , ((0, xF86XK_AudioLowerVolume), spawn "amixer -c PCH set Master 4-")
      -- , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")

      [ ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +4%"),
        ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -4%"),
        ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute 0 toggle"),
        ((shiftMask, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2+"),
        ((shiftMask, xF86XK_AudioLowerVolume), spawn "amixer set Master 2-"),
        ((modMask .|. shiftMask, xK_f), spawn (keybindScripts ++ "browser")),
        ((modMask .|. shiftMask .|. controlMask, xK_f), spawn (keybindScripts ++ "browser-private")),
        ((modMask .|. shiftMask .|. controlMask, xK_j), spawn (keybindScripts ++ "browser-jupyter")),
        -- NB: not in keybind dir
        ((modMask .|. shiftMask, xK_h), runInTerm "-title htop" "sh -c 'htop'"),
        ((0, xF86XK_MonBrightnessDown), spawn (keybindScripts ++ "backlight-down")),
        ((0, xF86XK_MonBrightnessUp), spawn (keybindScripts ++ "backlight-up")),
        ((modMask .|. shiftMask, xK_l), safeSpawnProg "xsecurelock"),
        ((modMask, xK_F12), spawn scrotMouse),
        ((modMask, xK_F11), spawn scrot),
        -- "Remote server access"
        ((modMask .|. shiftMask, xK_a), safeSpawnProg (keybindScripts ++ "ssh-remote-server")),
        -- "Remote server access"
        ((modMask .|. shiftMask, xK_d), safeSpawnProg (keybindScripts ++ "ssh-remote-server-2")),
        -- "Remote emacs server"
        ((modMask .|. shiftMask, xK_x), runInTerm "-title emacs-remote" (keybindScripts ++ "emacs-remote")),
        -- "Local emacs"
        ((modMask .|. shiftMask, xK_z), runInTerm "-title emacs" (keybindScripts ++ "emacs")),
        -- "Local emacs gtk"
        ((modMask .|. shiftMask .|. controlMask, xK_z), safeSpawnProg (keybindScripts ++ "emacs-gtk")),
        -- "Sound control"
        -- ((modMask .|. shiftMask, xK_s), runInTerm "-title mixer" (keybindScripts ++ "mixer"))
        -- "Sound control"
        ((modMask .|. shiftMask, xK_s), safeSpawnProg (keybindScripts ++ "mixer")),
        -- "Turn off external monitor"
        ((modMask .|. shiftMask, xK_n), spawn (keybindScripts ++ "monoff")),
        -- "Turn on external monitor"
        ((modMask .|. shiftMask, xK_m), spawn (keybindScripts ++ "mon")),
        ((modMask .|. shiftMask, xK_minus), sendMessage MirrorShrink),
        ((modMask .|. shiftMask, xK_equal), sendMessage MirrorExpand),
        -- KB: Win+Control+M        merge all windows in one SubLayout
        ((winMask .|. controlMask, xK_m), withFocused (sendMessage . MergeAll)),
        ((winMask .|. controlMask .|. shiftMask, xK_m), withFocused (sendMessage . UnMergeAll)),
        -- KB: Win+Control+U        unmerge focused window
        ((winMask .|. controlMask, xK_u), withFocused (sendMessage . UnMerge)),
        -- KB: Win+(Shift+)Tab      focus next/previous windows in a SubLayout
        ((winMask, xK_Tab), onGroup W.focusDown'),
        ((winMask .|. shiftMask, xK_Tab), onGroup W.focusUp'),
        --screenshot
        ((modMask .|. shiftMask .|. controlMask, xK_s), spawn (keybindScripts ++ "screenshot"))
      ]

-- https://web.archive.org/web/20120618020917/http://pastebin.com/EGifa2sg
basicLayout = Tall nmaster delta ratio
  where
    nmaster = 1
    delta = 3 / 100
    ratio = 1 / 2

tabbedLayout = named "tab" $ avoidStruts $ subTabbed basicLayout

--layout :: Choose ResizableTall (Choose (Mirror Tall) Full) a
layout =
  resizabletall
    ||| mirrortiled
    ||| Full
    ||| centeredIfSingle 0.6 1 tabbedLayout
  where
    resizabletall = ResizableTall 1 (3 / 100) (1 / 2) []
    mirrortiled = Mirror (Tall nmaster delta ratio)
    nmaster = 1 -- the default number of windows in the master pane
    ratio = 1 / 2 -- default proportion of screen occupied by master pane
    delta = 3 / 100 -- percent of screen to increment by when resizing panes

mhook :: ManageHook
mhook =
  composeOne
    [ isDialog -?> doCenterFloat,
      isNotification -?> doIgnore
    ]
    <+> composeAll [className =? "zoom" --> doFloat]

main :: IO ()
main =
  xmonad
    =<< xmobar
      DefaultConfig.def
        { terminal = "xterm",
          borderWidth = 0,
          keys = keyLayout,
          layoutHook = layout,
          manageHook = mhook
        }
