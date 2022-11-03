import XMonad
-- skipped
-- import XMonad.Hooks.DynamicLog
-- import XMonad.Hooks.ManageDocks
-- import XMonad.Util.Run(spawnPipe)
--


import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks

-- gnome panel
import XMonad.ManageHook

-- layout
import XMonad.Layout.Accordion
import XMonad.Layout.Grid

-- screen layout
import XMonad.Actions.PhysicalScreens
-- keyboard shortcuts for cycling windows/screens/
import XMonad.Actions.CycleWS

import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig

--  pager support
import XMonad.Hooks.EwmhDesktops
--
import XMonad.Config.Desktop


main = do
    xmonad $ ewmh defaultConfig {
        workspaces = myWorkspaces
        -- <+> is the Monoid mappend
        , manageHook = managementHooks <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ layoutHook defaultConfig ||| Accordion ||| Grid
        , startupHook = ewmhDesktopsStartup >> startupHook desktopConfig
        , modMask = mod4Mask
        , terminal = "xterm"
        } `additionalKeysP` myKeys

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

myKeys = [
         ("M1-C-<Right>", nextWS)
       , ("M1-C-S-<Right>", shiftToNext)
       , ("M1-C-<Left>", prevWS)
       , ("M1-C-S-<Left>", shiftToPrev)
       -- , ("M-<Right>", nextScreen)
       -- , ("M-<Left>", prevScreen)
       , ("M-z", toggleWS)
       , ("M-<Print>", spawn "xfce4-screenshooter")
       , ("M1-<Tab>", windows W.focusDown)

    ] ++

    -- this is list comprehenson, e.g.:
    -- ghci> [x*2 | x <- [1..10], x*2 >= 12]
    -- [12,14,16,18,20]
    [
      (otherModMasks ++ "M-" ++ [key], action tag) | (tag, key) <- zip myWorkspaces "123456789",
      (otherModMasks, action) <- [("", windows . W.view), ("S-", windows . W.shift)]
    ] ++

    -- fix multi-display ordering
    -- from https://wiki.haskell.org/Xmonad/Frequently_asked_questions#Screens_are_in_wrong_order
    [
      (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
    | (key, scr)  <- zip "wer" [1,0,2] -- was [0..] *** change to match your screen order ***
    , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
    ]


managementHooks = composeAll
  [
    resource =? "Do" --> doIgnore
  , className =? "Xfce4-notifyd" --> doIgnore -- notifications don't grab focus
  , className =? "rdesktop" --> doFloat
  , className =? "Steam" --> doFloat
  , className =? "steam" --> doFullFloat --bigpicture-mode
  , className =? "MainThrd" --> doFloat
  , className =? "LogicSniffer - Logic Analyzer Client" --> doFloat
  , className =? "LogicSniffer" --> doFloat
  , className =? "Logic" --> doFloat
  , className =? "Xfce4-whiskermenu" --> doFloat
  , className =? "mpv" --> doFloat
  , stringProperty "_NET_WM_NAME" =? "Emulator" --> doFloat -- android emulator (it doesn't have a className property)
  , title =? "plasma-desktop" --> doIgnore
  , manageDocks
  ]
