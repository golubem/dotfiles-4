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
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Actions.Navigation2D
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Actions.GridSelect

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

myWorkspaces = clickable $ ["web","dev","term","4","5","6","7","media","im"]
    where clickable l = ["<action=`xdotool key super+" ++ show n ++ "`>" ++ ws ++ "</action>" | (i,ws) <- zip [1..9] l, let n = i ]


------------------------------------------------------------------------
-- Prompt config
------------------------------------------------------------------------
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig { font        = "xft:Terminus:pixelsize=13"
                             , bgColor     = myBgColor
                             , fgColor     = myFgColor
                             , bgHLight    = myBgHLight
                             , fgHLight    = myFgHLight
                             , borderColor = myNormalBorderColor
                             }

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
------------------------------------------------------------------------

myKeys = [ ("M-f",                      sendMessage $ Toggle NBFULL)
         --[ ("M-f",                      withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f)
         , ("M-m",                      sendMessage ToggleStruts)
         , ("M1-<F4>",                  kill)
         , ("M-q",                      spawn "pkill -KILL xmobar || xmonad --recompile && xmonad --restart")
         , ("M-<F12>",                  spawn "xautolock -locknow")
         ---------------------------------------------------------------
         -- Navigation
         ---------------------------------------------------------------
         , ("M-<Left>",                 moveTo Prev $ WSIs notSP)
         , ("M-<Right>",                moveTo Next $ WSIs notSP)
         , ("M1-<Left>",                prevNonEmptyWS)
         , ("M1-<Right>",               nextNonEmptyWS)
         , ("M-S-<Left>",               shiftTo Prev (WSIs notSP) >> moveTo Prev (WSIs notSP))
         , ("M-S-<Right>",              shiftTo Next (WSIs notSP) >> moveTo Next (WSIs notSP))
         , ("M-g",                      goToSelected defaultGSConfig)

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
         ---------------------------------------------------------------
         -- Run applications
         ---------------------------------------------------------------
         , ("M-p",                      runOrRaisePrompt myXPConfig)
         , ("M-S-p",                    shellPrompt myXPConfig)
         , ("M-a",                      spawn "firefox")
         , ("M-s",                      spawn "spacefm")
         , ("M-d",                      spawn "urxvtc -e ranger")
         , ("M-z",                      namedScratchpadAction myScratchPads "terminal")
         , ("M-x",                      namedScratchpadAction myScratchPads "rt")
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
         , ("<XF86MonBrightnessUp>",    spawn "xbacklight -inc 10& ")
         , ("<XF86MonBrightnessDown>",  spawn "xbacklight -dec 10& ")
         , ("<XF86AudioRaiseVolume>",   spawn "amixer set Master 2%+")
         , ("<XF86AudioLowerVolume>",   spawn "amixer set Master 2%-")
         , ("<XF86AudioMute>",          spawn "amixer set Master toggle")
         ] where
                notSP = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)
                nextNonEmptyWS = findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1
                    >>= \t -> (windows . W.view $ t)
                prevNonEmptyWS = findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1
                    >>= \t -> (windows . W.view $ t)
                getSortByIndexNoSP =
                    fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
------------------------------------------------------------------------
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
    ]
-----------------------------------3------------------------------------
-- Layouts
------------------------------------------------------------------------

myLayout
    = smartBorders
    -- $ mkToggle (single NBFULL)
    $ onWorkspace (myWorkspaces !! 8) pidginLayot
    $ named "[T]" tiled |||
      named "[M]" (Mirror tiled) |||
      named "[F]" Full
    where
        pidginLayot = named "[I]" $ spacing 4 $ withIM (1%7) (Role "buddy_list") Grid
        tiled   = spacing 4 $ Tall nmaster delta ratio
        nmaster = 1
        ratio   = 1/2
        delta   = 3/100

------------------------------------------------------------------------
-- Window rules:
------------------------------------------------------------------------

myManageHook = composeAll
    [ manageDocks
    , namedScratchpadManageHook myScratchPads
    , className =? "Firefox"            --> doShift (myWorkspaces !! 0)
    , className =? "Plugin-container"   --> doFloat
    , className =? "Pidgin"             --> doShift (myWorkspaces !! 8)
    --, className =? "mpv"                --> doShift (myWorkspaces !! 7) >> doFloat
    , isDialog                          --> doFloat
    , resource  =? "desktop_window"     --> doIgnore
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
myNormalBorderColor     = "#1a1a1a"
myFocusedBorderColor    = "#2a2a2a"
myFgColor               = "#c5c8c6"
myBgColor               = "#1d1f21"
myBgHLight              = "#373b41"
myFgHLight              = "#c5c8c6"

yellowColor             = "#de935f"
blueColor               = "#5f819d"

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
                   , ppSort = fmap (.namedScratchpadFilterOutWorkspace) $ ppSort defaultPP
                   }

------------------------------------------------------------------------
-- Scratchpads
------------------------------------------------------------------------
myScratchPads = [ NS "terminal" "urxvtc -name 'scratchpad' -e bash -c 'tmux a -t scratchpad || tmux new -s scratchpad'" (resource =? "scratchpad") floatingTerm
                , NS "rt" "urxvtc -name 'rt' -e bash -c 'tmux a -t rt'" (resource =? "rt") floatingRt
                ]
                where
                    floatingTerm = customFloating $ W.RationalRect l t w h where
                        h = 0.4
                        w = 1
                        t = 1 - h
                        l = 1 - w
                    floatingRt = customFloating $ W.RationalRect l t w h where
                        h = 0.8
                        w = 1
                        t = 1 - h
                        l = 1 - w
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

--myNavigation2DConfig = defaultNavigation2DConfig { layoutNavigation     = [("Full", centerNavigation)]
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
    } `additionalKeysP` myKeys

