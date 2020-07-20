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
import XMonad.Actions.WindowBringer
import XMonad.Actions.WithAll

--import XMonad.Config.Prime
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
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Pass
import XMonad.Prompt.Window

import Graphics.X11.ExtraTypes.XF86

import qualified Data.Map as M
import qualified Data.HashTable.IO as H
import qualified XMonad.Actions.Search as S
import qualified XMonad.StackSet as W

-- ./lib
import XMonad.Actions.NoSPCycleWS
import XMonad.Hooks.Swallow
import XMonad.Layout.HorizontalMaster
import XMonad.Layout.MyPerScreen
import XMonad.Layout.MySpacing
import XMonad.Prompt.Tmux


------------------------------------------------------------------------
-- VARIABLES
--

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

-- mod1Mask: left alt key
-- mod4Mask: windows key
myModMask = mod1Mask

myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#1d99f3"
myBackgroundColor    = "#000000"
myForegroundColor    = "#ffffff"
myUrgentColor        = "#ff0000"
myCopyWindowColor    = "#00ff00"

myWorkspaces :: [String]
myWorkspaces =  ["1", "2", "3", "4" , "5", "6", "7", "8", "9"]


------------------------------------------------------------------------
-- THEME

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
-- TERMINAL
-- info: launching new terminal in current working directory
-- TODO: when/how do '/proc/pid/cwd' change?
--

mkTerm = withWindowSet launchTerminal

launchTerminal ws = case W.peek ws of
    Nothing -> runInTerm "" "$SHELL"
    Just xid -> terminalInCwd xid

terminalInCwd xid = let
    hex = showHex xid " "
    shInCwd = "'cd \"$(readlink /proc/$(ps --ppid $(xprop -id 0x" ++ hex ++ "_NET_WM_PID | cut -d\" \" -f3) -o pid= | tr -d \" \")/cwd)\" && $SHELL'"
    in runInTerm "" $ "sh -c " ++ shInCwd


------------------------------------------------------------------------
-- KEY BINDINGS
-- see: /usr/include/X11/keysymdef.h
--

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm,                   xK_Return   ), spawn $ XMonad.terminal conf )
    -- ((modm,                   xK_Return   ), mkTerm )
    , ((modm,                   xK_d        ), spawn "$HOME/.config/dmenu/scripts/dmenu_apps.sh" )
    , ((modm .|. shiftMask,     xK_d        ), spawn "sudo -A $HOME/.config/dmenu/scripts/dmenu_apps.sh" )
    , ((modm .|. shiftMask,     xK_period   ), spawn "$HOME/.config/dmenu/scripts/dmenu_edit.sh" )
    , ((modm .|. shiftMask,     xK_u        ), spawn "$HOME/.config/dmenu/scripts/dmenu_unicode.sh" )
    , ((modm .|. shiftMask,     xK_v        ), spawn "$HOME/.config/dmenu/scripts/dmenu_virtualbox.sh" )
    , ((modm .|. shiftMask,     xK_x        ), spawn "$HOME/.config/dmenu/scripts/dmenu_shutdown.sh" )
    , ((modm .|. shiftMask,     xK_o        ), spawn "$HOME/.config/dmenu/scripts/dmenu_scripts.sh" )
    , ((modm,                   xK_c        ), spawn "$HOME/.config/dmenu/scripts/dmenu_clipboard.sh" )
    , ((modm,                   xK_plus     ), spawn "$HOME/.local/bin/volume-control up 1" )
    , ((modm,                   xK_minus    ), spawn "$HOME/.local/bin/volume-control down 1" )
    , ((modm,                   xK_m        ), spawn "$HOME/.local/bin/volume-control toggle" )
    , ((modm,                   xK_p        ), spawn "$HOME/.local/bin/music-control toggle" )
    , ((modm,                   xK_n        ), spawn "$HOME/.local/bin/music-control next" )
    , ((modm .|. shiftMask,     xK_p        ), spawn "$HOME/.local/bin/music-control prev" )
    , ((modm .|. shiftMask,     xK_b        ), spawn "$HOME/.local/bin/x11-wallpaper choice" )
    , ((0,                      xK_Print    ), spawn "$HOME/.local/bin/screenshot" )
    , ((modm,                   xK_x        ), spawn "i3lock --nofork -B=100" )
    , ((modm,                   xK_space    ), sendMessage NextLayout )
    , ((modm .|. shiftMask,     xK_space    ), withFocused $ windows . W.sink )
    , ((modm,                   xK_f        ), sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL )
    , ((modm .|. shiftMask,     xK_Tab      ), setLayout $ XMonad.layoutHook conf )  -- reset to layout default setting
    --, ((modm,                   xK_r        ), refresh )
    , ((modm,                   xK_r        ), XMonad.Layout.MySpacing.triggerResizeEvent )
    --, ((modm .|. shiftMask,     xK_f        ), gotoMenuArgs ["-l", "20"] )
    , ((modm .|. shiftMask,     xK_f        ), windowPrompt myPromptTheme Goto allWindows )
    -- , ((modm .|. shiftMask,     xK_w        ), bringMenuArgs ["-l", "20"] )
    , ((modm .|. shiftMask,     xK_w        ), windowPrompt myPromptTheme Bring allWindows )
    , ((modm,                   xK_j        ), windows W.focusDown )
    , ((modm,                   xK_k        ), windows W.focusUp )
    , ((modm,                   xK_a        ), windows W.focusMaster )
    , ((modm .|. shiftMask,     xK_Return   ), dwmpromote ) -- swap focused window like dwm
    , ((modm .|. shiftMask,     xK_j        ), windows W.swapDown )
    , ((modm .|. shiftMask,     xK_k        ), windows W.swapUp )
    , ((modm,                   xK_h        ), sendMessage Shrink )
    , ((modm,                   xK_l        ), sendMessage Expand )
    , ((modm .|. shiftMask,     xK_plus     ), sendMessage (IncMasterN 1) )
    , ((modm .|. shiftMask,     xK_minus    ), sendMessage (IncMasterN (-1)) )
    , ((modm,                   xK_g        ), XMonad.Layout.MySpacing.toggleAuto16x9SpacingEnabled ) -- toggle widscreen gaps
    , ((modm,                   xK_s        ), windows copyToAll ) -- Make Sticky  (focused window)
    , ((modm .|. shiftMask,     xK_s        ), killAllOtherCopies ) -- Unset Sticky (focused window)
    , ((modm .|. shiftMask,     xK_t        ), namedScratchpadAction myScratchPads "terminal" )
    , ((modm .|. shiftMask,     xK_m        ), namedScratchpadAction myScratchPads "ncmpcpp" )
    , ((modm .|. shiftMask,     xK_n        ), namedScratchpadAction myScratchPads "newsboat" )
    , ((modm .|. shiftMask,     xK_a        ), namedScratchpadAction myScratchPads "pulsemixer" )
    , ((modm .|. shiftMask,     xK_i        ), namedScratchpadAction myScratchPads "htop" )
    , ((modm,                   xK_i        ), tagPrompt myPromptTheme (\s -> withFocused (addTag s)))
    , ((modm .|. shiftMask,     xK_g        ), tagPrompt myPromptTheme (\s -> withTaggedGlobalP s shiftHere))
    --, ((modm,                   xK_BackSpace), withFocused hideWindow )
    --, ((modm .|. shiftMask,     xK_BackSpace), popOldestHiddenWindow )
    , ((modm,                   xK_F1       ), sendMessage $ JumpToLayout "HL" )
    , ((modm,                   xK_F2       ), sendMessage $ JumpToLayout "HR" )
    , ((modm,                   xK_F3       ), sendMessage $ JumpToLayout "3ColMid" )
    , ((modm,                   xK_F4       ), sendMessage $ JumpToLayout "3Col" )
    , ((modm,                   xK_F5       ), sendMessage $ JumpToLayout "nCol" )
    , ((modm,                   xK_F6       ), sendMessage $ JumpToLayout "zROW" )
    , ((modm.|. shiftMask,      xK_F1       ), spawn "groff -mom ~/.local/share/dotfiles-readme.mom -tbl -Tpdf | zathura -" )
    , ((modm,                   xK_Right    ), sendMessage $ Go R)
    , ((modm,                   xK_Left     ), sendMessage $ Go L)
    , ((modm,                   xK_Up       ), sendMessage $ Go U)
    , ((modm,                   xK_Down     ), sendMessage $ Go D)
    , ((modm .|. controlMask,   xK_Right    ), sendMessage $ Swap R)
    , ((modm .|. controlMask,   xK_Left     ), sendMessage $ Swap L)
    , ((modm .|. controlMask,   xK_Up       ), sendMessage $ Swap U)
    , ((modm .|. controlMask,   xK_Down     ), sendMessage $ Swap D)
    , ((modm,                   xK_Page_Up  ), nextNonEmptyWS)
    , ((modm,                   xK_Page_Down), prevNonEmptyWS)
    , ((modm .|. shiftMask,     xK_Page_Up  ), shiftAndView Next)
    , ((modm .|. shiftMask,     xK_Page_Down), shiftAndView Prev)
    --, ((modm .|. shiftMask,     xK_Page_Up  ), sequence_[ shiftAndView Next, refresh])

    --, ((0,                   xK_Page_Up  ), nextNonEmptyWS)
    --, ((0,                   xK_Page_Down), prevNonEmptyWS)

    -- SubLayout
    --, ((modm .|. controlMask,   xK_t        ), sendMessage $ Toggle ENABLETABS)
    , ((modm .|. controlMask,   xK_h        ), sendMessage $ pullGroup L)
    , ((modm .|. controlMask,   xK_l        ), sendMessage $ pullGroup R)
    , ((modm .|. controlMask,   xK_k        ), sendMessage $ pullGroup U)
    , ((modm .|. controlMask,   xK_j        ), sendMessage $ pullGroup D)
    , ((modm .|. controlMask,   xK_m        ), withFocused (sendMessage . MergeAll))
    , ((modm .|. controlMask,   xK_u        ), withFocused (sendMessage . UnMerge))
    , ((modm .|. controlMask,   xK_comma    ), onGroup W.focusUp')
    , ((modm .|. controlMask,   xK_period   ), onGroup W.focusDown')

    -- Media Keys (Graphics.X11.ExtraTypes.XF86)
    , ((0, xF86XK_AudioLowerVolume          ), spawn "$HOME/.local/bin/volume-control down 1" )
    , ((0, xF86XK_AudioRaiseVolume          ), spawn "$HOME/.local/bin/volume-control up 1" )
    , ((0, xF86XK_AudioMute                 ), spawn "$HOME/.local/bin/volume-control toggle" )

    , ((modm .|. shiftMask,     xK_e        ), confirmPrompt warningPromptTheme "Quit XMonad" $ io (exitWith ExitSuccess) )
    , ((modm,                   xK_q        ), XMonad.Actions.CopyWindow.kill1 ) --  Remove the focused window from this workspace. If it is last window copy, then kill it instead.
    , ((modm .|. shiftMask,     xK_q        ), confirmPrompt warningPromptTheme "kill all" $ XMonad.Actions.WithAll.killAll ) -- Kill all windows on current workspace
    , ((modm .|. shiftMask,     xK_r        ), spawn "rm -rf ~/.xmonad; xmonad --recompile && xmonad --restart" )
    ]

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move focused window to workspace N
    -- mod-control-[1..9], Swap current workspaces with workspace N
    -- mod-control-shift-[1..9], Copy focused window to workspace N
    ++ [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (copy, shiftMask .|. controlMask), (W.shift, shiftMask), (swapWithCurrent, controlMask) ]]

    -- web search/translator
    ++ [((modm, xK_w), submap . M.fromList $ [((0, k), S.promptSearch myPromptTheme f) | (k,f) <- searchList ])]  -- search with prompt
    ++ [((modm .|. controlMask, xK_w), submap . M.fromList $ [((0, k), S.selectSearch f) | (k,f) <- searchList ])]  -- search from clipbard
    ++ [((modm, xK_t), submap . M.fromList $ [((0, k), S.promptSearch myPromptTheme f) | (k,f) <- translateList ])]  -- translate with prompt
    ++ [((modm .|. controlMask, xK_t), submap . M.fromList $ [((0, k), S.selectSearch f) | (k,f) <- translateList ])]  -- translate from clipbard

    -- pass prompt
    ++ [((modm .|. controlMask, xK_p), submap . M.fromList $ [((0, k), f myPromptTheme) | (k,f) <- [(xK_g, passPrompt), (xK_n, passGeneratePrompt)] ])]


------------------------------------------------------------------------
-- MOUSE BINDINGS
--

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm .|. shiftMask, button1), (\w -> XMonad.focus w
      >> mouseMoveWindow w
      >> ifClick (snapMagicMove (Just 50) (Just 50) w)  -- double-left-click to snap
      >> windows W.shiftMaster))

    , ((modm .|. shiftMask, button3), (\w -> XMonad.focus w
     --  >> setDefaultCursor xC_lr_angle -- does not work
      >> mouseResizeWindow w
      >> ifClick (snapMagicResize [R,D] (Just 50) (Just 50) w)  -- double-right-click to resize up to nearest border
      >> windows W.shiftMaster))
      -- >> setDefaultCursor xC_left_ptr))
    ]


------------------------------------------------------------------------
-- LAYOUTS
--
-- NOTE: If you change layout be sure to use 'setLayout $ XMonad.layoutHook conf' from keybindings after restarting
-- to reset your layout state to the new defaults, as xmonad preserves your old layout settings by default.
--

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
    tabsToggle x          = mkToggle (ENABLETABS ?? EOT) x  -- add this to enable toggle tabs (currently we have tabs allways enabled - required for my swallow extension)


------------------------------------------------------------------------
-- SCRATCHPADS
--

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

        spawnNcmpcpp = myTerminal ++ " --class ncmpcpp -t ncmpcpp -e ncmpcpp"
        findNcmpcpp = resource =? "ncmpcpp"
        manageNcmpcpp = ( noTaskbar <+> customFloating (W.RationalRect 0.25 0.1 0.5 0.8) )

        spawnNewsboat = myTerminal ++ " --class newsboat -t newsboat -e newsboat"
        findNewsboat = resource =? "newsboat"
        manageNewsboat = ( noTaskbar <+> customFloating (W.RationalRect 0.25 0.1 0.5 0.8) )

        spawnPulsemixer = myTerminal ++ " --class pulsemixer -t pulsemixer -e pulsemixer"
        findPulsemixer = resource =? "pulsemixer"
        managePulsemixer = ( noTaskbar <+> customFloating (W.RationalRect 0.25 0.1 0.5 0.8) )

        spawnHtop = myTerminal ++ " --class htop -t htop -e htop"
        findHtop = resource =? "htop"
        manageHtop = ( noTaskbar <+> customFloating (W.RationalRect 0.25 0.1 0.5 0.8) )


------------------------------------------------------------------------
-- WINDOW RULES
-- info: xprop: WM_NAME = title, second part of WM_CLASS = className, first part of WM_CLASS = resource
--

myWindowRules = composeAll
    [ isFullscreen                  --> doFullFloat
    , isDialog                      --> doCenterFloat
    , className =? "mpv"            --> doFloat
    , matchAny "no-focus"           --> doF W.focusDown
    , matchAny "pop-up"             --> doCenterFloat
    , matchAny "dialog"             --> doCenterFloat
    , matchAny "menu"               --> doCenterFloat
    , matchAny "center"             --> doCenterFloat
    , matchAny "floating"           --> doFloat <+> doF W.focusDown
    ]

    where
        matchAny :: String -> Query Bool
        matchAny s = className =? s <||> title =? s <||> resource =? s
        unfloat = ask >>= doF . W.sink


------------------------------------------------------------------------
-- SEARCH ENGINES
-- Xmonad contrib has several search engines available to use, located in
-- XMonad.Actions.Search. Additionally, you can add other search engines.

archwiki, reddit :: S.SearchEngine
archwiki = S.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="
reddit   = S.searchEngine "reddit" "https://www.reddit.com/search/?q="

deepl_de2en, deepl_en2de :: S.SearchEngine
deepl_de2en = S.searchEngine "deepl de2en" "https://www.deepl.com/translator#de/en/"
deepl_en2de = S.searchEngine "deepl en2de" "https://www.deepl.com/translator#en/de/"

searchList :: [(KeySym, S.SearchEngine)]
searchList = [ (xK_a, archwiki)
             , (xK_d, S.duckduckgo)
             , (xK_g, S.google)
             , (xK_i, S.images)
             , (xK_r, reddit)
             , (xK_y, S.youtube)
             ]

translateList :: [(KeySym, S.SearchEngine)]
translateList = [ (xK_e, deepl_de2en)
             , (xK_d, deepl_en2de)
             ]


------------------------------------------------------------------------
-- MANAGE HOOK
--

myManageHook = myWindowRules
            <+> xPropManageHook xPropMatches
            <+> namedScratchpadManageHook myScratchPads
            <+> manageDocks
            <+> manageHook desktopConfig

xPropMatches :: [XPropMatch]
xPropMatches = [ ([ (wM_CLASS, any ("firefox" ==)) ], pmX (addTag "browser")) ]


------------------------------------------------------------------------
-- STARTUP HOOK
--

myStartupHook = do
    setDefaultCursor xC_left_ptr  -- set mouse cursor
    spawn "pkill trayer; trayer --edge top --align right --padding 2 --distance 1 --widthtype request --SetDockType true --SetPartialStrut false --expand true --transparent true --alpha 0 --tint 0x000000 --height 22"


------------------------------------------------------------------------
-- LOG HOOK
--

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
        , ppTitle            = xmobarColor myForegroundColor "" . shorten 128
        , ppHidden           = check
        , ppOrder            = id
        , ppSep              = " : "
        , ppSort             = fmap (namedScratchpadFilterOutWorkspace.) (ppSort def)
    }


------------------------------------------------------------------------
-- URGENCY HOOK
--

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "Workspace" ++ idx]


------------------------------------------------------------------------
-- REMOVE BORDER
--

removeBorderQuery :: Query Bool
removeBorderQuery = className =? "mpv"

removeBorder :: Window -> X ()
removeBorder ws = withDisplay $ \d -> mapM_ (\w -> io $ setWindowBorderWidth d w 0) [ws]

myBorderEventHook :: Event -> X All

myBorderEventHook (ConfigureEvent {ev_window = window, ev_above  = above}) = do
    whenX (runQuery removeBorderQuery window) (removeBorder window)
    whenX (runQuery removeBorderQuery window) (refresh)  -- refresh required to get correct mpv floating position
    return $ All True

myBorderEventHook _ = return $ All True


------------------------------------------------------------------------
-- TEST AREA
--

-- debugPrint :: String -> IO ()
-- debugPrint = appendFile "/tmp/xmonad-debug"  -- debug on

-- removeBorderQuery :: Query Bool
-- removeBorderQuery = className =? "mpv"

-- removeBorder :: Window -> X ()
-- removeBorder ws = withDisplay $ \d -> mapM_ (\w -> io $ setWindowBorderWidth d w 0) [ws]

-- addBorder :: Window -> X ()
-- addBorder ws = withDisplay $ \d -> mapM_ (\w -> io $ setWindowBorderWidth d w myBorderWidth) [ws]

-- myBorderEventHook :: Event -> X All

-- myBorderEventHook (PropertyEvent {ev_window = window, ev_atom = a, ev_propstate = s}) = do
--     foc <- withWindowSet $ return . W.peek
--     when (foc /= Just window) $ (removeBorder window)
--     when (foc == Just window) $ (addBorder window)
--     return $ All True

-- myBorderEventHook (ConfigureEvent {ev_window = window, ev_above  = above}) = do
--     refresh
--     return $ All True

-- myBorderEventHook e@(AnyEvent {ev_event_type=t, ev_window=win})
--         | t == focusIn = do
--                    addBorder window
--                    return $ All True
--         | t == focusOut = do
--                    removeBorder window
--                    return $ All True
--         | otherwise = return $ All True

-- myBorderEventHook _ = return $ All True


------------------------------------------------------------------------
-- MAIN
--

main = do
    xmproc <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc"
    pidHashTable <- H.new :: IO(H.BasicHashTable Int Window)
    windowHashTable <- H.new :: IO(H.BasicHashTable Window Int)
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
        --clientMask = structureNotifyMask .|. enterWindowMask .|. propertyChangeMask .|. focusChangeMask,
        mouseBindings      = myMouseBindings,
        layoutHook         = myLayout,
        startupHook        = myStartupHook,
        handleEventHook    = swallowEventHook pidHashTable windowHashTable <+> myBorderEventHook
        --handleEventHook    = mempty
    }

