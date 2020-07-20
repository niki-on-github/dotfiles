
-----------------------------------------------------------------------------
--
-- Module      :  XMonad.Prompt.Temux
-- Copyright   :  (c) none
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  none
-- Stability   :  unstable
-- Portability :  unportable
--
-- Tmux session xmonad prompt
--
-----------------------------------------------------------------------------

module XMonad.Prompt.Tmux
    ( -- * Usage
      -- $usage
      tmuxSessionPrompt
    , tmuxSessionGrid
    ) where

import XMonad
import Control.Monad
import System.IO
import XMonad.Actions.Search
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Actions.GridSelect
import XMonad.Util.Run

-- $usage
--
-- > import XMonad.Prompt
-- > import XMonad.Prompt.Tmux
--
-- > myKeys = ...
-- >    -- Select an existing session with grid.
-- >    , ((modm              , xK_semicolon), tmuxSessionGrid)
-- >    -- Specify session to attach or create with prompt.
-- >    , ((modm .|. shiftMask, xK_semicolon), tmuxSessionPrompt myPromptTheme)
-- >    ...
--

escapeShellArg :: String -> String
escapeShellArg src = "\"" ++ (escape src) ++ "\""
    where escape = foldr escapeChar ""
          escapeChar x = case x of
            '\\' -> ("\\\\" ++)
            '"'  -> ("\\\"" ++)
            x    -> (x :)

mltermTmuxSession :: String -> X ()
mltermTmuxSession s = spawn $ "alacritty -e sh -c " ++ (escapeShellArg $ "tmux -2 attach-session -t " ++ name ++ " || tmux -2 new-session -s " ++ name)
    where name = escapeShellArg s

-- Get tmux sessions.
tmuxSessionsList :: MonadIO m => m [String]
tmuxSessionsList = lines `liftM` runProcessWithInput "tmux" ["list-sessions", "-F", "#S"] ""

-- Get tmux sessions and its status.
tmuxSessionsList' :: MonadIO m => m [(String, String)]
tmuxSessionsList' = (map proc . lines) `liftM` runProcessWithInput "tmux" ["list-sessions", "-F", "#{session_attached} #S"] ""
    where proc str = (sname str, sstat str)
          sname = tail . dropWhile (/= ' ')
          sstat str = if (isPrefixOf "0 " str)
                         then "detached"
                         else "attached"

tmuxSessionGrid :: X ()
tmuxSessionGrid = map genAction `liftM` tmuxSessionsList' >>= runSelectedAction def
    where genAction (sname, sstat) = (sname ++ " [" ++ sstat ++ "]", mltermTmuxSession sname)

tmuxSessionPrompt :: XPConfig -> X ()
tmuxSessionPrompt promptTheme =
    tmuxSessionsList >>=
    (\ss -> inputPromptWithCompl promptTheme "tmux session" (mkComplFunFromList' ss)
            ?+ mltermTmuxSession)
