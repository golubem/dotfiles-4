import XMonad
import Data.Monoid

import XMonad.Hooks.SetWMName                   -- to fix java's grey windows
import XMonad.Hooks.EwmhDesktops                -- to automaticly expand fullscreen apps
import XMonad.Layout.Fullscreen                 -- to manualy expend app to fullscreen
import XMonad.Layout.Named
import XMonad.Util.EZConfig                     -- ez shortcuts
import XMonad.Hooks.DynamicLog                  -- for bar
import XMonad.Hooks.ManageDocks
import XMonad.Actions.CycleWS

import XMonad.Layout.IM                         -- layout for pidgin
import XMonad.Layout.Grid                       -- grid layout
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.SimplestFloat

import XMonad.Util.Run
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition

import XMonad.Actions.WindowGo
import XMonad.Actions.Navigation2D

import Data.Ratio ((%))
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified XMonad.Actions.FlexibleResize as Flex

myTerminal      = "urxvtc"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth   = 2
myModMask       = mod4Mask
--myBrowser       = "qutebrowser"
myBrowser       = "firefox"

myWorkspaces = clickable ["web","dev","term","4","5","6","7","media","im"]
    where clickable l = ["<action=`xdotool key super+" ++ i ++ "`>" ++ ws ++ "</action>" | (i,ws) <- zip keymap l ]
          keymap = ["U26", "U5B", "U7B", "U7D", "U28", "U3D", "U2A", "U29", "U2B"]


------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
------------------------------------------------------------------------

myKeys = [ ("M-f",                      sendMessage $ Toggle NBFULL)
         , ("M-m",                      sendMessage ToggleStruts)
         , ("M1-<F4>",                  kill)
         , ("M-r",                      spawn "pkill -KILL xmobar || xmonad --recompile && xmonad --restart")
         , ("M-<F12>",                  spawn "xautolock -locknow")
         ---------------------------------------------------------------
         -- Navigation
         ---------------------------------------------------------------
         , ("M-h",                      moveTo Prev $ WSIs notSP)
         , ("M-l",                      moveTo Next $ WSIs notSP)
         , ("M-p",                      prevNonEmptyWS)
         , ("M-n",                      nextNonEmptyWS)
         , ("M-S-h",                    shiftTo Prev (WSIs notSP) >> moveTo Prev (WSIs notSP))
         , ("M-S-l",                    shiftTo Next (WSIs notSP) >> moveTo Next (WSIs notSP))

         , ("M1-<Tab>",                 windows W.focusDown >> windows W.shiftMaster)
         , ("M1-S-<Tab>",               windows W.focusUp >> windows W.shiftMaster)
         , ("M1-j",                     windows W.focusDown)
         , ("M1-k",                     windows W.focusUp)
         , ("M1-h",                     windowGo L False)
         , ("M1-l",                     windowGo R False)

         , ("M1-S-j",                   windowSwap D False)
         , ("M1-S-k",                   windowSwap U False)
         , ("M1-S-h",                   windowSwap L False)
         , ("M1-S-l",                   windowSwap R False)

         , ("M-S-j",                    sendMessage Shrink)
         , ("M-S-k",                    sendMessage Expand)

         ---------------------------------------------------------------
         -- Run applications
         ---------------------------------------------------------------
         , ("M-<Return>",               spawn "dmenu.sh")
         , ("M-a",                      runOrRaise "firefox" (className =? "Firefox"))
         , ("<XF86HomePage>",           runOrRaise "firefox" (className =? "Firefox"))
         , ("M-e",                      spawn "spacefm")
         , ("M-u",                      spawn $ myTerminal ++ " -e ranger")
         , ("<XF86MyComputer>",         spawn $ myTerminal ++ " -e ranger")
         , ("M-o",                      spawn $ myTerminal ++ " -e nvim")
         , ("M-'",                      namedScratchpadAction myScratchPads "terminal")
         , ("M-q",                      namedScratchpadAction myScratchPads "torrent")

         ---------------------------------------------------------------
         -- MPD
         ---------------------------------------------------------------
         , ("C-M1-<Up>",                spawn "mpc toggle")
         , ("C-M1-<Down>",              spawn "mpc stop")
         , ("C-M1-<Left>",              spawn "mpc prev")
         , ("C-M1-<Right>",             spawn "mpc next")

         ---------------------------------------------------------------
         -- Media keys
         ---------------------------------------------------------------
         , ("<XF86MonBrightnessUp>",    spawn "light -A 10")
         , ("<XF86MonBrightnessDown>",  spawn "light -U 10")
         , ("<XF86AudioRaiseVolume>",   spawn "amixer set Master 2%+")
         , ("<XF86AudioLowerVolume>",   spawn "amixer set Master 2%-")
         , ("<XF86AudioMute>",          spawn "amixer set Master toggle")
         , ("<XF86Explorer>",           spawn "touchpad.sh -t")
         , ("<XF86LaunchA>",            spawn "kbd.sh -t")


         ] ++
         [ (m ++ [k], windows $ f i)
         | (i, k) <- zip myWorkspaces "&[{}(=*)+"
         , (f, m) <- [(W.greedyView, "M-"), (W.shift, "M-S-")]
         ] ++
         [ (m ++ [key], screenWorkspace sc >>= flip whenJust (windows . f))
         | (key, sc) <- zip ";,." [0..]
         , (f, m) <- [(W.view, "M-"), (W.shift, "M-S-")]]
         where
                notSP = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)
                nextNonEmptyWS = findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1
                    >>= \t -> windows . W.view $ t
                prevNonEmptyWS = findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1
                    >>= \t -> windows . W.view $ t
                getSortByIndexNoSP =
                    fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
------------------------------------------------------------------------
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                      >> windows W.shiftMaster)
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    , ((modm, button3), \w -> focus w >> Flex.mouseResizeWindow w)
    ]
------------------------------------------------------------------------
-- Layouts
------------------------------------------------------------------------

myLayout
    = smartBorders
    -- $ mkToggle (single NBFULL)
    $ onWorkspace (myWorkspaces !! 8) pidginLayot
    $ named "[T]" tiled |||
      named "[M]" (Mirror tiled) |||
      named "[G]" grid |||
      named "[F]" Full
    where
        pidginLayot = named "[I]" $ spacing 4 $ withIM (1%7) (Role "buddy_list") Grid
        tiled       = spaces $ Tall nmaster delta ratio
        grid        = spaces Grid
        spaces      = spacing 4
        nmaster     = 1
        ratio       = 1/2
        delta       = 3/100

------------------------------------------------------------------------
-- Window rules
------------------------------------------------------------------------

myManageHook = composeAll
    [ manageDocks
    , namedScratchpadManageHook myScratchPads
    , className =? myBrowser            --> doShift (head myWorkspaces)
    , className =? "Plugin-container"   --> doFloat
    , className =? "Octave-gui"         --> doFloat
    , className =? "NeercGame"          --> doFloat
    , className =? "Pidgin"             --> doShift (myWorkspaces !! 8)
    , className =? "mpv"                --> doFloat
    , isDialog                          --> doFloat
    , resource  =? "desktop_window"     --> doIgnore
    , className =? "steam"              --> doShift (myWorkspaces !! 6)
    ]
    <+>
    composeOne [isFullscreen -?> doFullFloat]

------------------------------------------------------------------------
-- Event handling
------------------------------------------------------------------------
myEventHook = mempty

------------------------------------------------------------------------
-- Colors
------------------------------------------------------------------------
myNormalBorderColor     = "#202020"
myFocusedBorderColor    = "#606060"
myFgColor               = "#c5c8c6"
myBgColor               = "#1d1f21"
myBgHLight              = "#202020"
myFgHLight              = "#fff0f0"

yellowColor             = "#f0c674"
blueColor               = "#81a2be"

------------------------------------------------------------------------
-- Status bars and logging
------------------------------------------------------------------------
myLogHook xmproc = dynamicLogWithPP $ xmobarPP
                   { ppOutput = hPutStrLn xmproc
                   , ppCurrent = xmobarColor blueColor myBgColor
                   , ppUrgent = xmobarColor "#202020" "#ac4142"
                   --, ppVisible = xmobarColor "#90a959" "#151515"
                   , ppSep = " : "
                   , ppLayout = xmobarColor "#d0d0d0" ""
                   , ppOrder = \(ws:l:t:_) -> [" " ++ l,ws,t]
                   , ppTitle = xmobarColor "#d0d0d0" "" . shorten 140
                   , ppSort = (. namedScratchpadFilterOutWorkspace) <$> ppSort defaultPP
                   }

------------------------------------------------------------------------
-- Scratchpads
------------------------------------------------------------------------
myScratchPads = [ NS "terminal" "urxvtc -name 'scratchpad' -e bash -c 'tmux a -t scratchpad || tmux new -s scratchpad'" (resource =? "scratchpad") floatingTerm
                , NS "torrent" "transmission-gtk" (stringProperty "WM_NAME" =? "Transmission") floatingRt
                ]
                where
                    floatingTerm = customFloating $ W.RationalRect l t w h where
                        h = 0.4
                        w = 1
                        t = 1 - h
                        l = 1 - w
                    floatingRt = customFloating $ W.RationalRect l t w h where
                        h = 1
                        w = 0.3
                        t = 1 - h
                        l = 0
------------------------------------------------------------------------
-- Startup hook
------------------------------------------------------------------------
myStartupHook = setWMName "LG3D"
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
------------------------------------------------------------------------

main = do
    xmproc <- spawnPipe ".cabal/bin/xmobar 2> ~/err"
    xmonad $ defaults xmproc

--myNavigation2DConfig = defaultNavigation2DConfig { layoutNavigation     = [("Full", centerNavigation)`2]
--                                                 , unmappedWindowRect   = [("Full", singleWindowRect)]
--                                                 }
defaults xmproc = defaultConfig
    { terminal            = myTerminal
      ,focusFollowsMouse  = myFocusFollowsMouse
      ,clickJustFocuses   = myClickJustFocuses
      ,borderWidth        = myBorderWidth
      ,modMask            = myModMask
      ,workspaces         = myWorkspaces
      ,normalBorderColor  = myNormalBorderColor
      ,focusedBorderColor = myFocusedBorderColor
      ,mouseBindings      = myMouseBindings
      ,layoutHook         = mkToggle (single NBFULL) $ avoidStruts myLayout
      ,manageHook         = fullscreenManageHook <+> myManageHook
      ,handleEventHook    = XMonad.Hooks.EwmhDesktops.fullscreenEventHook <+> myEventHook
      ,logHook            = myLogHook xmproc
      ,startupHook        = myStartupHook
      --,keys               = myKeys
    } `additionalKeysP` myKeys
