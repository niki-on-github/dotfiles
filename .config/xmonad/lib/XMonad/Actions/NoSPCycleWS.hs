-----------------------------------------------------------------------------
--
-- Module      :  XMonad.Actions.NoSPCycleWS
-- Copyright   :  (c) none
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  none
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides functions for bindings to cycle forward or backward through the
-- list of workspaces, to move windows between workspaces, and to cycle
-- between screens. In contrast to Actions.CycleWS implementation this one
-- ignores the scratchpad workspace.
--
-----------------------------------------------------------------------------

module XMonad.Actions.NoSPCycleWS ( shiftAndView, nextNonEmptyWS, prevNonEmptyWS ) where

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.OnScreen
import XMonad.Layout.IndependentScreens
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
import qualified XMonad.StackSet as W


-- XMonad.Actions.CycleWS functions which ignore Scratchpad
shiftAndView dir = findWorkspace getSortByIndexNoSP dir (hiddenWS :&: emptyWS :&: currentScreenWS) 1
        >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)
nextNonEmptyWS = findWorkspace getSortByIndexNoSP Next (hiddenWS :&: Not emptyWS :&: currentScreenWS) 1
        >>= \t -> (windows . W.view $ t)
prevNonEmptyWS = findWorkspace getSortByIndexNoSP Prev (hiddenWS :&: Not emptyWS :&: currentScreenWS) 1
        >>= \t -> (windows . W.view $ t)
getSortByIndexNoSP =
        fmap (.filterOutWs [scratchpadWorkspaceTag]) getSortByIndex

isOnScreen :: ScreenId -> WindowSpace -> Bool
isOnScreen s ws = s == unmarshallS (W.tag ws)

currentScreen :: X ScreenId
currentScreen = gets (W.screen . W.current . windowset)

currentScreenWS :: WSType
currentScreenWS = WSIs $ do
     s <- currentScreen
     return $ \x -> W.tag x /= "NSP" && isOnScreen s x
