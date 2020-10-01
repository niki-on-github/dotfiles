{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts #-}

import XMonad hiding ( (|||) )

import Control.Monad
import Data.Monoid
import Data.List
import Data.Maybe
import Numeric
import System.Exit

import XMonad.Actions.CopyWindow
import XMonad.Actions.DwmPromote
import XMonad.Actions.FloatSnap
import XMonad.Actions.Submap
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.TagWindows
import XMonad.Actions.Warp
import XMonad.Actions.WindowBringer
import XMonad.Actions.WithAll

import XMonad.Config.Desktop

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.XPropManage

import XMonad.Layout.BoringWindows
import XMonad.Layout.Column
import XMonad.Layout.Gaps
import XMonad.Layout.Groups
import XMonad.Layout.Hidden
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation
import XMonad.Layout.ZoomRow

import XMonad.Util.Cursor
import XMonad.Util.NoTaskbar
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.Paste
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Pass
import XMonad.Prompt.Window

import Graphics.X11.ExtraTypes.XF86

import qualified Data.Map as M
import qualified XMonad.Actions.Search as S
import qualified XMonad.StackSet as W

-- ./lib
import XMonad.Actions.NoSPCycleWS
import XMonad.Layout.HorizontalMaster
import XMonad.Layout.MyPerScreen
import XMonad.Layout.MySpacing
import XMonad.Prompt.Tmux


------------------------------------------------------------------------
-- VARIABLES
-- Description: configuration of terminal, fonts, statusbar height, gaps, colors, workspace names, ...

-- NOTE: Some terminal parameters in this configuration are specific to the alacritty terminal!
myTerminal = "alacritty"

myFont = "xft:Noto Sans Mono:pixelsize=16:antialias=true:hinting=true"
mySmalFont = "xft:Noto Sans Mono:pixelsize=12:antialias=true:hinting=true"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth = 2
myGaps = 4
myStatusbarHeight = 27

-- NOTE: mod1Mask: left alt key, mod4Mask: windows key
myModMask = mod1Mask

myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#1d99f3"
myBackgroundColor    = "#000000"
myForegroundColor    = "#ffffff"
myUrgentColor        = "#ff0000"
myCopyWindowColor    = "#00ff00"

myWorkspaces :: [String]
myWorkspaces =  ["1", "2", "3", "4" , "5", "6", "7", "8", "9"]

trayerCommand :: String
trayerCommand = "trayer --edge top --align right --padding 2 --distance 1 --widthtype request --SetDockType true --SetPartialStrut false --expand true --transparent true --alpha 0 --tint 0x171717 --height 22"

xmobarToggleCommand :: String
xmobarToggleCommand = "dbus-send --session --dest=org.Xmobar.Control --type=method_call '/org/Xmobar/Control' org.Xmobar.Control.SendSignal \"string:Toggle 0\""


------------------------------------------------------------------------
-- THEME
-- Description: my prompt and tab theme

myPromptTheme = def
    { font                  = myFont
    , bgColor               = myBackgroundColor
    , fgColor               = myForegroundColor
    , bgHLight              = myFocusedBorderColor
    , fgHLight              = myForegroundColor
    , borderColor           = myBackgroundColor
    , promptBorderWidth     = 0
    , height                = myStatusbarHeight
    , position              = Top
    , showCompletionOnTab   = False
    , alwaysHighlight       = True
    , historySize           = 0
    , searchPredicate       = fuzzyMatch
    , sorter                = fuzzySort
}

warningPromptTheme = myPromptTheme
    { bgColor               = myUrgentColor
    , fgColor               = myForegroundColor
}

myTabTheme :: XMonad.Layout.Tabbed.Theme
myTabTheme = def
    { fontName              = mySmalFont
    , activeColor           = myFocusedBorderColor
    , inactiveColor         = myBackgroundColor
    , urgentColor           = myBackgroundColor
    , activeBorderColor     = myFocusedBorderColor
    , inactiveBorderColor   = myFocusedBorderColor
    , urgentBorderColor     = myUrgentColor
    , activeTextColor       = myForegroundColor
    , inactiveTextColor     = myForegroundColor
    , urgentTextColor       = myUrgentColor
}


------------------------------------------------------------------------
-- KEY BINDINGS
-- Description: my keyboard bindings
-- Note: see /usr/include/X11/keysymdef.h

ifFocusedWindowClass :: String -> X () -> X () -> X ()
ifFocusedWindowClass cname thenX elseX = withFocused $ \windowId -> do
    c <- runQuery className windowId
    if c == cname then thenX else elseX

ifFocusedWindowFloating :: X () -> X () ->  X ()
ifFocusedWindowFloating thenX elseX = withFocused $ \windowId -> do
    floats <- gets (W.floating . windowset)
    if windowId `M.member` floats then thenX else elseX

ifFloating :: Window -> X () -> X () ->  X ()
ifFloating w thenX elseX = do
    floats <- gets (W.floating . windowset)
    if w `M.member` floats then thenX else elseX

toggleFloat :: X ()
toggleFloat = ifFocusedWindowFloating (withFocused $ windows . W.sink) (withFocused $ float)

toggleFloatCenter :: Window -> X ()
toggleFloatCenter w = windows (\s -> if M.member w (W.floating s)
    then W.sink w s else (W.float w (W.RationalRect 0.25 0.1 0.5 0.8) s))

toggleSticky :: X ()
toggleSticky = do
    copies <- wsContainingCopies
    if null copies then windows copyToAll else killAllOtherCopies

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm,                   xK_Return   ), spawn $ XMonad.terminal conf )
    , ((modm,                   xK_d        ), spawn "$HOME/.config/dmenu/scripts/dmenu_apps.sh" )
    , ((modm .|. shiftMask,     xK_d        ), spawn "sudo -A $HOME/.config/dmenu/scripts/dmenu_apps.sh" )
    , ((modm .|. shiftMask,     xK_period   ), spawn "$HOME/.config/dmenu/scripts/dmenu_edit.sh" )
    , ((modm .|. shiftMask,     xK_u        ), spawn "$HOME/.config/dmenu/scripts/dmenu_unicode.sh" )
    , ((modm .|. shiftMask,     xK_v        ), spawn "$HOME/.config/dmenu/scripts/dmenu_virtualbox.sh" )
    , ((modm .|. shiftMask,     xK_x        ), spawn "$HOME/.config/dmenu/scripts/dmenu_shutdown.sh" )
    , ((modm .|. shiftMask,     xK_o        ), spawn "$HOME/.config/dmenu/scripts/dmenu_scripts.sh" )
    , ((modm,                   xK_c        ), spawn "$HOME/.config/dmenu/scripts/dmenu_clipboard.sh" )
    , ((modm .|. shiftMask,     xK_BackSpace), spawn "$HOME/.config/dmenu/scripts/dmenu_kill.sh" )
    , ((modm,                   xK_w        ), spawn "$HOME/.config/dmenu/scripts/dmenu_firefox.sh" )
    , ((modm,                   xK_plus     ), spawn "$HOME/.local/bin/volume-control up 1" )
    , ((modm,                   xK_minus    ), spawn "$HOME/.local/bin/volume-control down 1" )
    , ((modm,                   xK_m        ), spawn "$HOME/.local/bin/volume-control toggle" )
    , ((modm,                   xK_p        ), spawn "$HOME/.local/bin/music-control toggle" )
    , ((modm,                   xK_n        ), spawn "$HOME/.local/bin/music-control next" )
    , ((modm .|. shiftMask,     xK_p        ), spawn "$HOME/.local/bin/music-control prev" )
    , ((modm .|. shiftMask,     xK_b        ), spawn "$HOME/.local/bin/x11-wallpaper choice" )
    , ((0,                      xK_Print    ), spawn "$HOME/.local/bin/screenshot" )
    , ((shiftMask,              xK_Print    ), spawn "flameshot gui" )
    , ((modm,                   xK_x        ), spawn "$HOME/.local/bin/x11-lock --fast" )
    , ((modm,                   xK_space    ), sendMessage NextLayout )
    , ((modm .|. shiftMask,     xK_space    ), withFocused $ windows . W.sink )
    , ((modm,                   xK_f        ), ifFocusedWindowClass "mpv" (ifFocusedWindowFloating (sendKey noModMask xK_f) (sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL)) (sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL) )
    , ((modm .|. shiftMask,     xK_f        ), withFocused toggleFloatCenter )
    , ((modm .|. shiftMask,     xK_Tab      ), setLayout $ XMonad.layoutHook conf )  -- reset to layout default setting
    , ((modm,                   xK_r        ), XMonad.Layout.MySpacing.triggerResizeEvent )
    , ((modm .|. controlMask,   xK_f        ), gotoMenuArgs ["-l", "20"] )
    , ((modm .|. shiftMask,     xK_w        ), bringMenuArgs ["-l", "20"] )
    , ((modm,                   xK_j        ), sequence_[ windows W.focusDown, warpToWindow 0.5 1 ] )
    , ((modm,                   xK_k        ), sequence_[ windows W.focusUp, warpToWindow 0.5 1 ] )
    , ((modm,                   xK_a        ), windows W.focusMaster )
    , ((modm .|. shiftMask,     xK_Return   ), dwmpromote )  -- swap focused window like dwm
    , ((modm,                   xK_b        ), spawn (xmobarToggleCommand ++ " && pkill trayer || " ++ trayerCommand) ) -- toggle xmobar + trayer
    , ((modm .|. shiftMask,     xK_j        ), windows W.swapDown )
    , ((modm .|. shiftMask,     xK_k        ), windows W.swapUp )
    , ((modm,                   xK_h        ), sendMessage Shrink )
    , ((modm,                   xK_l        ), sendMessage Expand )
    , ((modm .|. shiftMask,     xK_plus     ), sendMessage (IncMasterN 1) )
    , ((modm .|. shiftMask,     xK_minus    ), sendMessage (IncMasterN (-1)) )
    , ((modm,                   xK_g        ), XMonad.Layout.MySpacing.toggleAuto16x9SpacingEnabled )  -- toggle widscreen gaps
    , ((modm,                   xK_s        ), toggleSticky )
    , ((modm .|. shiftMask,     xK_t        ), namedScratchpadAction myScratchPads "terminal" )
    , ((modm .|. shiftMask,     xK_m        ), namedScratchpadAction myScratchPads "ncmpcpp" )
    , ((modm .|. shiftMask,     xK_n        ), namedScratchpadAction myScratchPads "newsboat" )
    , ((modm .|. shiftMask,     xK_a        ), namedScratchpadAction myScratchPads "pulsemixer" )
    , ((modm .|. shiftMask,     xK_i        ), namedScratchpadAction myScratchPads "htop" )
    , ((modm,                   xK_F1       ), sendMessage $ JumpToLayout "HL" )
    , ((modm,                   xK_F2       ), sendMessage $ JumpToLayout "HR" )
    , ((modm,                   xK_F3       ), sendMessage $ JumpToLayout "3ColMid" )
    , ((modm,                   xK_F4       ), sendMessage $ JumpToLayout "3Col" )
    , ((modm,                   xK_F5       ), sendMessage $ JumpToLayout "nCol" )
    , ((modm,                   xK_F6       ), sendMessage $ JumpToLayout "zROW" )
    , ((modm.|. shiftMask,      xK_F1       ), spawn "groff -mom ~/.local/share/dotfiles-readme.mom -tbl -Tpdf | zathura -" )
    , ((modm,                   xK_Right    ), sequence_[ sendMessage $ Go R, warpToWindow 0.5 1 ])
    , ((modm,                   xK_Left     ), sequence_[ sendMessage $ Go L, warpToWindow 0.5 1 ])
    , ((modm,                   xK_Up       ), ifFocusedWindowClass "Code" (sendKey (modm) xK_Up) (sequence_[ sendMessage $ Go U, warpToWindow 0.5 1 ]))  -- allow swap lines in vs code
    , ((modm,                   xK_Down     ), ifFocusedWindowClass "Code" (sendKey (modm) xK_Down) (sequence_[ sendMessage $ Go D, warpToWindow 0.5 1 ]))  -- allow swap line in vs code
    , ((modm .|. controlMask,   xK_Right    ), sequence_[ sendMessage $ Swap R, warpToWindow 0.5 1 ])
    , ((modm .|. controlMask,   xK_Left     ), sequence_[ sendMessage $ Swap L, warpToWindow 0.5 1 ])
    , ((modm .|. controlMask,   xK_Up       ), sequence_[ sendMessage $ Swap U, warpToWindow 0.5 1 ])
    , ((modm .|. controlMask,   xK_Down     ), sequence_[ sendMessage $ Swap D, warpToWindow 0.5 1 ])
    , ((modm,                   xK_Page_Up  ), nextNonEmptyWS)
    , ((modm,                   xK_Page_Down), prevNonEmptyWS)
    , ((modm .|. shiftMask,     xK_Page_Up  ), shiftAndView Next)
    , ((modm .|. shiftMask,     xK_Page_Down), shiftAndView Prev)

    -- fix
    , ((controlMask .|. shiftMask, xK_c), ifFocusedWindowClass "firefox" (sendKey (controlMask) xK_c) (sendKey (controlMask .|. shiftMask) xK_c) )

    -- SubLayout
    , ((modm .|. shiftMask .|. controlMask, xK_h     ), sendMessage $ pullGroup L)
    , ((modm .|. shiftMask .|. controlMask, xK_l     ), sendMessage $ pullGroup R)
    , ((modm .|. shiftMask .|. controlMask, xK_k     ), sendMessage $ pullGroup U)
    , ((modm .|. shiftMask .|. controlMask, xK_j     ), sendMessage $ pullGroup D)
    , ((modm .|. shiftMask .|. controlMask, xK_Left  ), sendMessage $ pullGroup L)
    , ((modm .|. shiftMask .|. controlMask, xK_Right ), sendMessage $ pullGroup R)
    , ((modm .|. shiftMask .|. controlMask, xK_Up    ), sendMessage $ pullGroup U)
    , ((modm .|. shiftMask .|. controlMask, xK_Down  ), sendMessage $ pullGroup D)
    , ((modm .|. shiftMask .|. controlMask, xK_m     ), withFocused (sendMessage . MergeAll))
    , ((modm .|. shiftMask .|. controlMask, xK_u     ), withFocused (sendMessage . UnMerge))
    , ((modm .|. controlMask,   xK_comma    ), onGroup W.focusUp')
    , ((modm .|. controlMask,   xK_period   ), onGroup W.focusDown')

    -- Media Keys (Graphics.X11.ExtraTypes.XF86)
    , ((0, xF86XK_AudioLowerVolume          ), spawn "$HOME/.local/bin/volume-control down 1" )
    , ((0, xF86XK_AudioRaiseVolume          ), spawn "$HOME/.local/bin/volume-control up 1" )
    , ((0, xF86XK_AudioMute                 ), spawn "$HOME/.local/bin/volume-control toggle" )

    -- xmonad control
    , ((modm .|. shiftMask,     xK_e        ), confirmPrompt warningPromptTheme "Quit XMonad" $ io (exitWith ExitSuccess) )
    , ((modm,                   xK_q        ), XMonad.Actions.CopyWindow.kill1 ) --  Remove the focused window from this workspace. If it is last window copy, then kill it instead.
    , ((modm .|. shiftMask,     xK_q        ), confirmPrompt warningPromptTheme "kill all" $ XMonad.Actions.WithAll.killAll ) -- Kill all windows on current workspace
    , ((modm .|. shiftMask,     xK_r        ), spawn "xmonad --recompile && xmonad --restart" )
    ]

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move focused window to workspace N
    -- mod-control-[1..9], Swap current workspaces with workspace N
    -- mod-control-shift-[1..9], Copy focused window to workspace N
    ++ [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (copy, shiftMask .|. controlMask), (W.shift, shiftMask), (swapWithCurrent, controlMask) ]]


------------------------------------------------------------------------
-- MOUSE BINDINGS
-- Description: my mouse bindings to move and resize floating windows

sendMouseClickToWindow :: Window -> Button -> X ()
sendMouseClickToWindow win btn = safeSpawn "xdotool" ["click", "--window", show win, show btn]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm, button1), (\w -> XMonad.focus w
      >> ifFloating w (mouseMoveWindow w
      >> ifClick (snapMagicMove (Just 50) (Just 50) w)  -- double-left-click to snap
      >> windows W.shiftMaster) (sendMouseClickToWindow w button1)))

    , ((modm, button3), (\w -> XMonad.focus w
      >> ifFloating w (mouseResizeWindow w
      >> ifClick (snapMagicResize [R,D] (Just 50) (Just 50) w)  -- double-right-click to resize up to nearest border
      >> windows W.shiftMaster) (sendMouseClickToWindow w button3)))
    ]


------------------------------------------------------------------------
-- LAYOUTS
-- Description: my layouts for the widescreen monitor and standard screen which autimaticaly switch depending on the screen ratio
-- Note: If you change layout be sure to use 'setLayout $ XMonad.layoutHook conf' from keybindings after restarting
-- to reset your layout state to the new defaults, as xmonad preserves your old layout settings by default.

enableTabs x = renamed [(XMonad.Layout.Renamed.CutWordsLeft 1)] $ addTabs shrinkText myTabTheme $ subLayout [] Simplest x
data ENABLETABS = ENABLETABS deriving (Read, Show, Eq, Typeable)
instance Transformer ENABLETABS Window where
    transform ENABLETABS x k = k (enableTabs x) (const x)

myLayout = prefixed "[" $ suffixed "]" $ navigation $ avoidStruts $ smartBorders $ fullScreenToggle $ enableTabs $ enableHiddenWindows $ layoutSelector wideLayouts standardLayouts

  where
    -- first layout is default
    wideLayouts     = wHL ||| wHR ||| wThreeColMid ||| wThreeCol ||| wMulCol ||| wRow
    standardLayouts = tall ||| tabs ||| mirrorTall

    -- wide screen Layouts
    wHL                   = named "HL"        $ widescreenGaps $ ColMasterLeft 2 (2/100) (2/3)
    wHR                   = named "HR"        $ widescreenGaps $ ColMasterRight 2 (2/100) (2/3)
    wThreeColMid          = named "3ColMid"   $ widescreenGaps $ ThreeColMid 1 (2/100) (2/5)
    wThreeCol             = named "3Col"      $ widescreenGaps $ ThreeCol 1 (2/100) (2/5)
    wMulCol               = named "nCol"      $ widescreenGaps $ multiCol [1] 3 (2/100) (2/5)
    wRow                  = named "zROW"      $ widescreenGaps $ zoomRow

    -- standard Layouts
    tall                  = named "TALL"      $ standardGaps $ Tall 1 (2/100) (2/3)
    mirrorTall            = named "MTALL"     $ standardGaps $ Mirror $ Tall 1 (2/100) (2/3)
    tabs                  = named "TAB"       $ tabbed shrinkText myTabTheme  -- do not add gaps/spacing

    -- functions
    named n               = renamed [(XMonad.Layout.Renamed.Replace n)]
    suffixed n            = renamed [(XMonad.Layout.Renamed.Append n)]
    prefixed n            = renamed [(XMonad.Layout.Renamed.Prepend n)]
    silent                = renamed [(XMonad.Layout.Renamed.CutWordsLeft 1)]
    enableHiddenWindows x = silent $ hiddenWindows x
    widescreenGaps        = XMonad.Layout.MySpacing.auto16x9Spacing True (Border myGaps myGaps myGaps myGaps) (Border myGaps myGaps myGaps myGaps)
    standardGaps          = XMonad.Layout.Spacing.spacingRaw True (Border halfGaps halfGaps halfGaps halfGaps) True (Border myGaps myGaps myGaps myGaps) True
    halfGaps              = (div myGaps 2) + 1
    layoutSelector        = XMonad.Layout.MyPerScreen.ifWideScreen
    navigation x          = configurableNavigation noNavigateBorders $ boringWindows x
    fullScreenToggle x    = mkToggle (single FULL) x


------------------------------------------------------------------------
-- SCRATCHPADS
-- Description: the definition of my scratchpad applications
-- Note: some terminal application does not resize if called direct, workaround add sleep

myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "ncmpcpp" spawnNcmpcpp findNcmpcpp manageNcmpcpp
                , NS "newsboat" spawnNewsboat findNewsboat manageNewsboat
                , NS "pulsemixer" spawnPulsemixer findPulsemixer managePulsemixer
                , NS "htop" spawnHtop findHtop manageHtop
                ]
    where
        spawnTerm  = myTerminal ++ " --class scratchpad-terminal -t scratchpad-terminal"
        findTerm   = resource =? "scratchpad-terminal"
        manageTerm = ( noTaskbar <+> customFloating (W.RationalRect 0.25 0.1 0.5 0.8) )

        spawnNcmpcpp = myTerminal ++ " --class ncmpcpp -t ncmpcpp -e sh -c 'sleep 0.1; ncmpcpp;'"
        findNcmpcpp = resource =? "ncmpcpp"
        manageNcmpcpp = ( noTaskbar <+> customFloating (W.RationalRect 0.25 0.1 0.5 0.8) )

        spawnNewsboat = myTerminal ++ " --class newsboat -t newsboat -e sh -c 'sleep 0.1; newsboat;'"
        findNewsboat = resource =? "newsboat"
        manageNewsboat = ( noTaskbar <+> customFloating (W.RationalRect 0.25 0.1 0.5 0.8) )

        spawnPulsemixer = myTerminal ++ " --class pulsemixer -t pulsemixer -e sh -c 'sleep 0.1; pulsemixer;'"
        findPulsemixer = resource =? "pulsemixer"
        managePulsemixer = ( noTaskbar <+> customFloating (W.RationalRect 0.25 0.1 0.5 0.8) )

        spawnHtop = myTerminal ++ " --class htop -t htop -e sh -c 'sleep 0.1; htop;'"
        findHtop = resource =? "htop"
        manageHtop = ( noTaskbar <+> customFloating (W.RationalRect 0.25 0.1 0.5 0.8) )


------------------------------------------------------------------------
-- WINDOW RULES
-- Description: my window rules
-- Note: WM_NAME = title, second part of WM_CLASS = className, first part of WM_CLASS = resource

myWindowRules = composeAll
    [ isFullscreen                  --> doFullFloat
    , isDialog                      --> doCenterFloat
    , matchAny "floating"           --> doFloat <+> doF W.focusDown
    , className =? "mpv"            --> doFloat
    , matchAny "no-focus"           --> doF W.focusDown
    , matchAny "pop-up"             --> doCenterFloat
    , matchAny "dialog"             --> doCenterFloat
    , matchAny "menu"               --> doCenterFloat
    , matchAny "center"             --> doCenterFloat
    ]

    where
        matchAny :: String -> Query Bool
        matchAny s = className =? s <||> title =? s <||> resource =? s
        unfloat = ask >>= doF . W.sink


------------------------------------------------------------------------
-- MANAGE HOOK
-- Description: list of my management hooks

myManageHook = myWindowRules
            <+> xPropManageHook xPropMatches
            <+> namedScratchpadManageHook myScratchPads
            <+> manageDocks
            <+> manageHook desktopConfig

xPropMatches :: [XPropMatch]
xPropMatches = [ ([ (wM_CLASS, any ("firefox" ==)) ], pmX (addTag "browser")) ]


------------------------------------------------------------------------
-- STARTUP HOOK
-- Description: my startup application for the xmonad environment

myStartupHook = do
    setDefaultCursor xC_left_ptr  -- set mouse cursor
    spawn ("pkill trayer; sleep 1 && " ++ trayerCommand) -- load trayer


------------------------------------------------------------------------
-- LOG HOOK
-- Description: configuration of xmobar: color, formatting, clickable workspaces, ...

clickWorkspace :: String -> String -> String -> String
clickWorkspace a b ws = "<action=xdotool key alt+" ++ show(index) ++ ">" ++ a ++ ws ++ b ++ "</action>" where
	wsIdxToString Nothing = "1"
	wsIdxToString (Just n) = show (n+1)
	index = wsIdxToString (elemIndex ws myWorkspaces)

myLogHook h = do

    --color workspaces with copy windows instances in diffrent color
    copies <- wsContainingCopies
    let check ws | ws `elem` copies = xmobarColor myCopyWindowColor "" . clickWorkspace " " "*" $ ws
                 | otherwise        = xmobarColor myForegroundColor "" . clickWorkspace " " "*" $ ws

    -- xmobar config
    dynamicLogWithPP xmobarPP {
        ppOutput = hPutStrLn h
        , ppCurrent          = xmobarColor myForegroundColor myFocusedBorderColor . clickWorkspace " " " "
        , ppVisible          = xmobarColor myForegroundColor "" . clickWorkspace " " " "
        , ppHiddenNoWindows  = xmobarColor myForegroundColor "" . clickWorkspace " " " "
        , ppUrgent           = xmobarColor myUrgentColor     "" . clickWorkspace " " "*"
        , ppLayout           = xmobarColor myForegroundColor "" . wrap "<action=xdotool key alt+space>" "</action>"
        , ppTitle            = xmobarColor myForegroundColor "" . shorten 128
        , ppHidden           = check
        , ppOrder            = id
        , ppSep              = " : "
        , ppSort             = fmap (namedScratchpadFilterOutWorkspace.) (ppSort def)
    }


------------------------------------------------------------------------
-- URGENCY HOOK
-- Description: show notification when an app reports urgency (e.g. host enter the WebEx session)

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset
        safeSpawn "notify-send" [show name, "Workspace" ++ idx]


------------------------------------------------------------------------
-- FULLSCREEN EVENT HOOK
-- Description: automatically leave the fullscreen mode when the fullscreen app has been closed
-- Note: the current code triggers the close fullscren event also when closing dialogs of the main window
-- TODO: how can this behaviour be improved? (for gimp a workaround is currently implemented)

layoutName :: Query String
layoutName = liftX $ gets (description . W.layout . W.workspace . W.current . windowset)

isFullscreenQuery :: Query Bool
isFullscreenQuery = layoutName =? "[Full]"

myFullscreenEventHook :: Event -> X All

myFullscreenEventHook (DestroyWindowEvent {ev_event = eventId, ev_window = window}) = do
    -- query on properties of destroyed window is not possible, so we check the new focused window
    withFocused $ \w -> do
        cname <- runQuery className w
        -- first ceck that the event is actually about closing a window
        when (eventId == window) $ when (cname /= "Gimp") $ whenX (runQuery isFullscreenQuery window) (sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL)
    return $ All True

myFullscreenEventHook _ = return $ All True


------------------------------------------------------------------------
-- REMOVE BORDER
-- Description: remove the border from floating mpv

removeBorderQuery :: Query Bool
removeBorderQuery = className =? "mpv"

removeBorder :: Window -> X ()
removeBorder ws = withDisplay $ \d -> mapM_ (\w -> io $ setWindowBorderWidth d w 0) [ws]

addSmartBorder :: Window -> X ()
addSmartBorder ws = do
    winCount <- length . W.index . windowset <$> get
    if winCount > 1 then withDisplay $ \d -> mapM_ (\w -> io $ setWindowBorderWidth d w myBorderWidth) [ws] else return()

myBorderEventHook :: Event -> X All

myBorderEventHook (ConfigureEvent {ev_window = window, ev_above = above}) = do
    whenX (runQuery removeBorderQuery window) (ifFloating window (removeBorder window) (addSmartBorder window))
    -- refresh required to get correct mpv position
    whenX (runQuery removeBorderQuery window) (refresh)
    return $ All True

myBorderEventHook _ = return $ All True


------------------------------------------------------------------------
-- ROUND CORNER
-- Description: add round corner to floating windows (require picom with `rounded-corners-exclude = [ "_XMONAD_TAGS@:s != 'round-corner'" ];`)

myRoundCornerEventHook :: Event -> X All

myRoundCornerEventHook (ConfigureEvent {ev_window = window, ev_above = above}) = do
    ifFloating window (addTag "round-corner" window) (delTag "round-corner" window)
    return $ All True

myRoundCornerEventHook _ = return $ All True


------------------------------------------------------------------------
-- MAIN
-- Description: here the most important function, the main function

main = do
    xmproc <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc"
    xmonad $ docks $ withUrgencyHook LibNotifyUrgencyHook $ ewmh desktopConfig {
        manageHook         = myManageHook,
        XMonad.workspaces  = myWorkspaces,
        logHook            = myLogHook xmproc,
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        keys               = myKeys,
        clientMask         = clientMask def .|. focusChangeMask,
        mouseBindings      = myMouseBindings,
        layoutHook         = myLayout,
        startupHook        = myStartupHook,
        handleEventHook    = myBorderEventHook <+> myRoundCornerEventHook <+> myFullscreenEventHook
    }

