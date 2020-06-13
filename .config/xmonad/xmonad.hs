{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts #-}

import XMonad hiding ( (|||) )

import Control.Monad
import Data.Monoid
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
import XMonad.Hooks.Swallow
import XMonad.Layout.HorizontalMaster
import XMonad.Layout.MyPerScreen
import XMonad.Layout.MySpacing
import XMonad.Util.MyHelpers


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
    --((modm,                   xK_Return   ), mkTerm )
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
    , ((modm,                   xK_r        ), refresh )
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
    , ((modm,                   xK_t        ), withFocused $ windows . W.sink )
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
    , ((modm,                   xK_BackSpace), withFocused hideWindow )
    , ((modm .|. shiftMask,     xK_BackSpace), popOldestHiddenWindow )
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
    
    -- SubLayout 
    , ((modm .|. controlMask,   xK_t        ), sendMessage $ Toggle ENABLETABS)
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
    
    -- NOTE: XDG_CONFIG_HOME not work when recompiling has errors because it create a xmonad.errors in ~/.xmonad and then xmonad do search for the xmonad.hs in ~/.xmonad. Workaround:
    , ((modm .|. shiftMask,     xK_r        ), spawn "mkdir -p $HOME/.xmonad; cp -rf $HOME/.config/xmonad/* $HOME/.xmonad; xmonad --recompile && xmonad --restart" )

    ]

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move focused window to workspace N
    -- mod-control-[1..9], Swap current workspaces with workspace N
    -- mod-control-shift-[1..9], Copy focused window to workspace N
    ++ [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (copy, shiftMask .|. controlMask), (W.shift, shiftMask), (swapWithCurrent, controlMask) ]]
    
    -- web search engines
    ++ [((modm, xK_w), submap . M.fromList $ [((0, k), S.promptSearch myPromptTheme f) | (k,f) <- searchList ])]  -- search with prompt
    ++ [((modm .|. controlMask, xK_w), submap . M.fromList $ [((0, k), S.selectSearch f) | (k,f) <- searchList ])]  -- search from clipbard
    
    -- pass prompt
    ++ [((modm .|. controlMask, xK_p), submap . M.fromList $ [((0, k), f myPromptTheme) | (k,f) <- [(xK_g, passPrompt), (xK_n, passGeneratePrompt)] ])]


------------------------------------------------------------------------
-- MOUSE BINDINGS
--

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm,               button3) ,(\w -> XMonad.focus w
      >> mouseMoveWindow w
      >> ifClick (snapMagicMove (Just 50) (Just 50) w)
      >> windows W.shiftMaster))

    , ((modm .|. shiftMask, button3), (\w -> XMonad.focus w
      >> mouseResizeWindow w
      >> ifClick (snapMagicResize [R,D] (Just 50) (Just 50) w)
      >> windows W.shiftMaster))
    ]


------------------------------------------------------------------------
-- LAYOUTS
--
-- NOTE: If you change layout be sure to use 'setLayout $ XMonad.layoutHook conf' from keybindings after restarting 
-- to reset your layout state to the new defaults, as xmonad preserves your old layout settings by default.
--
-- TODO: set better layout names

enableTabs x = addTabs shrinkText myTabTheme $ subLayout [] Simplest x
data ENABLETABS = ENABLETABS deriving (Read, Show, Eq, Typeable)
instance Transformer ENABLETABS Window where
     transform ENABLETABS x k = k (enableTabs x) (const x)

myLayout = prefixed "[" $ suffixed "]" $ windowNavigation $ boringWindows $ avoidStruts $ smartBorders $ fullScreenToggle $ silent $ enableTabs $ silent $ hiddenWindows $ XMonad.Layout.MyPerScreen.ifWideScreen wideLayouts standardLayouts 

  where
    -- first layout is default
     wideLayouts     = wHL ||| wHR ||| wThreeColMid ||| wThreeCol ||| wMulCol ||| wRow
     standardLayouts = tall ||| tabs ||| mirrorTall

    -- wide screen Layouts
     wHL                = named "HL"        $ widescreenGaps $ ColMasterLeft 2 (2/100) (2/3)
     wHR                = named "HR"        $ widescreenGaps $ ColMasterRight 2 (2/100) (2/3)
     wThreeColMid       = named "3ColMid"   $ widescreenGaps $ ThreeColMid 1 (2/100) (2/5)
     wThreeCol          = named "3Col"      $ widescreenGaps $ ThreeCol 1 (2/100) (2/5)
     wMulCol            = named "nCol"      $ widescreenGaps $ multiCol [1] 3 (2/100) (2/5)
     wRow               = named "zROW"      $ widescreenGaps $ zoomRow
     
    -- standard Layouts
     tall               = named "TALL"      $ standardGaps $ Tall 1 (2/100) (2/3)
     mirrorTall         = named "MTALL"     $ standardGaps $ Mirror $ Tall 1 (2/100) (2/3)
     tabs               = named "TAB"       $ tabbed shrinkText myTabTheme  -- do not add gaps/spacing

    -- functions
     named n            = renamed [(XMonad.Layout.Renamed.Replace n)]
     suffixed n         = renamed [(XMonad.Layout.Renamed.Append n)]
     prefixed n         = renamed [(XMonad.Layout.Renamed.Prepend n)]
     silent             = renamed [(XMonad.Layout.Renamed.CutWordsLeft 1)]
     widescreenGaps     = XMonad.Layout.MySpacing.auto16x9Spacing True (Border myGaps myGaps myGaps myGaps) (Border myGaps myGaps myGaps myGaps)
     standardGaps       = XMonad.Layout.Spacing.spacingRaw True (Border halfGaps halfGaps halfGaps halfGaps) True (Border myGaps myGaps myGaps myGaps) True
     halfGaps           = (div myGaps 2) + 1
     fullScreenToggle x = mkToggle (single FULL) x
     tabsToggle x       = mkToggle (ENABLETABS ?? EOT) x  -- add this to enable toggle tabs (currently we have tabs allways enabled - required for my swallow extension)
   

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
-- WORKSPACES
-- info: My workspaces in xmobar are clickable (requires xdotool)
--

xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]
        
myWorkspaces :: [String]   
myWorkspaces = clickable . (map xmobarEscape)
        -- we use the space and star symbol to increase the clickabel area.
        -- in xmobarPP we replace the star symbol with a space if no window exist on this workspace
        $ [" 1*", " 2*", " 3*", " 4*" , " 5*", " 6*", " 7*", " 8*", " 9*"]
  where           
        -- NOTE: set your Modkey to alt or super (windows key)
        clickable l = [ "<action=xdotool key alt+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip [1..9] l, let n = i ]


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

searchList :: [(KeySym, S.SearchEngine)]
searchList = [ (xK_a, archwiki)
             , (xK_d, S.duckduckgo)
             , (xK_g, S.google)
             , (xK_i, S.images)
             , (xK_r, reddit)
             , (xK_y, S.youtube)
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
    spawn "pkill stalonetray; stalonetray -c $HOME/.config/xmobar/stalonetrayrc"


------------------------------------------------------------------------
-- LOG HOOK
--

myLogHook h = do
    -- color workspaces with copy windows instances in diffrent color
    copies <- wsContainingCopies
    let check ws | ws `elem` copies =
           xmobarColor myCopyWindowColor "" $ ws
         | otherwise = xmobarColor myForegroundColor "" $ ws
    -- xmobar config
    dynamicLogWithPP xmobarPP { 
        ppOutput = hPutStrLn h
        -- we replace the star symbol from workspace name with a space if no window exist on this workspace 
        , ppCurrent = xmobarColor myForegroundColor myFocusedBorderColor . map (\c -> if c=='*' then ' '; else c)
        , ppVisible = xmobarColor myForegroundColor ""
        , ppHidden = check
        , ppHiddenNoWindows = xmobarColor myForegroundColor "" . map (\c -> if c=='*' then ' '; else c)
        , ppTitle = xmobarColor myForegroundColor "" . shorten 128
        , ppSep =  "   "
        , ppUrgent = xmobarColor myUrgentColor ""
        , ppOrder = id
        , ppExtras = []
        , ppSort = fmap (namedScratchpadFilterOutWorkspace.) (ppSort def)
    }


------------------------------------------------------------------------
-- URGENCY HOOK
--

replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map $ \c -> if c == a then b else c

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset 

        safeSpawn "notify-send" [show name, "Workspace" ++ replace '*' ' ' idx]


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
        mouseBindings      = myMouseBindings,
        layoutHook         = myLayout,
        startupHook        = myStartupHook,
        handleEventHook    = swallowEventHook pidHashTable windowHashTable
        --handleEventHook    = mempty
    } 
    
