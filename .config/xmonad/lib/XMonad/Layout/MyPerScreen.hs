{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
--
-- Module      :  XMonad.Layout.MyPerScreen
-- Copyright   :  (c) Edward Z. Yang
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  none
-- Stability   :  unstable
-- Portability :  unportable
-- Fork of     :  XMonad.Layout.PerScreen
--
-- Select available layouts based on the width of your screen: 
-- use horizontal master layouts for wide screens (e.g 21:9) and 
-- vertical master layouts for obsolete screen ratios (e.g. 16:9).
-- 
-----------------------------------------------------------------------------

module XMonad.Layout.MyPerScreen
    ( -- * Usage
      -- $usage
      MyPerScreen,
      ifWideScreen
    ) where

import XMonad
import qualified XMonad.StackSet as W

import Data.Maybe (fromMaybe)

-- $usage
--
-- > import XMonad.Layout.MyPerScreen
--
-- > layoutHook = avoidStruts $ ifWideScreen (wideLayouts) (standardLayouts)
-- >   where
-- >      wideLayouts     = zoomRow ||| ThreeCol 1 (2/100) (2/5) ||| ThreeColMid 1 (2/100) (2/5)
-- >      standardLayouts = Tall 1 (2/100) (2/3) ||| Full
--

ifWideScreen :: (LayoutClass l1 a, LayoutClass l2 a)
               => (l1 a)      -- ^ layout to use when screen is a wide screen e.g 21:9 screen
               -> (l2 a)      -- ^ layout to use otherwise e.g 16:9 screen
               -> MyPerScreen l1 l2 a
ifWideScreen = MyPerScreen False

data MyPerScreen l1 l2 a = MyPerScreen Bool (l1 a) (l2 a) deriving (Read, Show)

-- | Construct new MyPerScreen values with possibly modified layouts.
mkNewMyPerScreenT :: MyPerScreen l1 l2 a -> Maybe (l1 a) ->
                      MyPerScreen l1 l2 a
mkNewMyPerScreenT (MyPerScreen _ lt lf) mlt' =
    (\lt' -> MyPerScreen True lt' lf) $ fromMaybe lt mlt'

mkNewMyPerScreenF :: MyPerScreen l1 l2 a -> Maybe (l2 a) ->
                      MyPerScreen l1 l2 a
mkNewMyPerScreenF (MyPerScreen _ lt lf) mlf' =
    (\lf' -> MyPerScreen False lt lf') $ fromMaybe lf mlf'

instance (LayoutClass l1 a, LayoutClass l2 a, Show a) => LayoutClass (MyPerScreen l1 l2) a where
    runLayout (W.Workspace i p@(MyPerScreen _ lt lf) ms) r
        | (2*rect_height r) < rect_width r  = do (wrs, mlt') <- runLayout (W.Workspace i lt ms) r
                                                 return (wrs, Just $ mkNewMyPerScreenT p mlt')
        | otherwise                         = do (wrs, mlt') <- runLayout (W.Workspace i lf ms) r
                                                 return (wrs, Just $ mkNewMyPerScreenF p mlt')

    handleMessage (MyPerScreen bool lt lf) m
        | bool      = handleMessage lt m >>= maybe (return Nothing) (\nt -> return . Just $ MyPerScreen bool nt lf)
        | otherwise = handleMessage lf m >>= maybe (return Nothing) (\nf -> return . Just $ MyPerScreen bool lt nf)

    description (MyPerScreen True  l1 _) = description l1
    description (MyPerScreen _     _ l2) = description l2
