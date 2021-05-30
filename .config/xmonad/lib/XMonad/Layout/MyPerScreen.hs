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
-- vertical layouts for vertical screens.
--
-----------------------------------------------------------------------------

module XMonad.Layout.MyPerScreen
    ( -- * Usage
      -- $usage
      MyPerVerticalScreen,
      MyPerWideScreen,
      ifWideScreen,
      ifVerticalScreen
    ) where

import XMonad
import qualified XMonad.StackSet as W

import Data.Maybe (fromMaybe)

-- $usage
--
-- > import XMonad.Layout.MyPerVerticalScreen
--
-- > layoutHook = avoidStruts $ ifWideScreen (wideLayouts) (standardLayouts)
-- >   where
-- >      wideLayouts     = zoomRow ||| ThreeCol 1 (2/100) (2/5) ||| ThreeColMid 1 (2/100) (2/5)
-- >      standardLayouts = Tall 1 (2/100) (2/3) ||| Full
--

ifWideScreen :: (LayoutClass l1 a, LayoutClass l2 a)
               => (l1 a)      -- ^ layout to use when screen is a wide screen e.g 21:9 screen
               -> (l2 a)      -- ^ layout to use otherwise e.g 16:9 screen
               -> MyPerWideScreen l1 l2 a
ifWideScreen = MyPerWideScreen False

ifVerticalScreen :: (LayoutClass l1 a, LayoutClass l2 a)
               => (l1 a)      -- ^ layout to use when screen is a wide screen e.g 21:9 screen
               -> (l2 a)      -- ^ layout to use otherwise e.g 16:9 screen
               -> MyPerVerticalScreen l1 l2 a
ifVerticalScreen = MyPerVerticalScreen False

data MyPerVerticalScreen l1 l2 a = MyPerVerticalScreen Bool (l1 a) (l2 a) deriving (Read, Show)

-- | Construct new MyPerVerticalScreen values with possibly modified layouts.
mkNewMyPerVerticalScreenT :: MyPerVerticalScreen l1 l2 a -> Maybe (l1 a) ->
                      MyPerVerticalScreen l1 l2 a
mkNewMyPerVerticalScreenT (MyPerVerticalScreen _ lt lf) mlt' =
    (\lt' -> MyPerVerticalScreen True lt' lf) $ fromMaybe lt mlt'

mkNewMyPerVerticalScreenF :: MyPerVerticalScreen l1 l2 a -> Maybe (l2 a) ->
                      MyPerVerticalScreen l1 l2 a
mkNewMyPerVerticalScreenF (MyPerVerticalScreen _ lt lf) mlf' =
    (\lf' -> MyPerVerticalScreen False lt lf') $ fromMaybe lf mlf'

instance (LayoutClass l1 a, LayoutClass l2 a, Show a) => LayoutClass (MyPerVerticalScreen l1 l2) a where
    runLayout (W.Workspace i p@(MyPerVerticalScreen _ lt lf) ms) r
        | (rect_height r) > (rect_width r)  = do (wrs, mlt') <- runLayout (W.Workspace i lt ms) r
                                                 return (wrs, Just $ mkNewMyPerVerticalScreenT p mlt')
        | otherwise                         = do (wrs, mlt') <- runLayout (W.Workspace i lf ms) r
                                                 return (wrs, Just $ mkNewMyPerVerticalScreenF p mlt')

    handleMessage (MyPerVerticalScreen bool lt lf) m
        | bool      = handleMessage lt m >>= maybe (return Nothing) (\nt -> return . Just $ MyPerVerticalScreen bool nt lf)
        | otherwise = handleMessage lf m >>= maybe (return Nothing) (\nf -> return . Just $ MyPerVerticalScreen bool lt nf)

    description (MyPerVerticalScreen True  l1 _) = description l1
    description (MyPerVerticalScreen _     _ l2) = description l2

data MyPerWideScreen l1 l2 a = MyPerWideScreen Bool (l1 a) (l2 a) deriving (Read, Show)

-- | Construct new MyPerWideScreen values with possibly modified layouts.
mkNewMyPerWideScreenT :: MyPerWideScreen l1 l2 a -> Maybe (l1 a) ->
                      MyPerWideScreen l1 l2 a
mkNewMyPerWideScreenT (MyPerWideScreen _ lt lf) mlt' =
    (\lt' -> MyPerWideScreen True lt' lf) $ fromMaybe lt mlt'

mkNewMyPerWideScreenF :: MyPerWideScreen l1 l2 a -> Maybe (l2 a) ->
                      MyPerWideScreen l1 l2 a
mkNewMyPerWideScreenF (MyPerWideScreen _ lt lf) mlf' =
    (\lf' -> MyPerWideScreen False lt lf') $ fromMaybe lf mlf'

instance (LayoutClass l1 a, LayoutClass l2 a, Show a) => LayoutClass (MyPerWideScreen l1 l2) a where
    runLayout (W.Workspace i p@(MyPerWideScreen _ lt lf) ms) r
        | (2*rect_height r) < rect_width r  = do (wrs, mlt') <- runLayout (W.Workspace i lt ms) r
                                                 return (wrs, Just $ mkNewMyPerWideScreenT p mlt')
        | otherwise                         = do (wrs, mlt') <- runLayout (W.Workspace i lf ms) r
                                                 return (wrs, Just $ mkNewMyPerWideScreenF p mlt')

    handleMessage (MyPerWideScreen bool lt lf) m
        | bool      = handleMessage lt m >>= maybe (return Nothing) (\nt -> return . Just $ MyPerWideScreen bool nt lf)
        | otherwise = handleMessage lf m >>= maybe (return Nothing) (\nf -> return . Just $ MyPerWideScreen bool lt nf)

    description (MyPerWideScreen True  l1 _) = description l1
    description (MyPerWideScreen _     _ l2) = description l2
