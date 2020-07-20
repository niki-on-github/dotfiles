{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances, DeriveDataTypeable #-}

-----------------------------------------------------------------------------
--
-- Module      :  XMonad.Layout.HorizontalMaster
-- Copyright   :  (c) none
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  none
-- Stability   :  ultra stable
-- Portability :  unportable
--
-- The windows in the master area are arranged in colums of equal size. The
-- number of columns is always nmaster + 1, and the last column is a stack of
-- leftover windows just like the normal tile layout (Tall). It effectively
-- acts like the default tiling mode, except provides for vertical instead
-- of horizontal master windows.
--
-----------------------------------------------------------------------------

module XMonad.Layout.HorizontalMaster ( ColMasterLeft(..), ColMasterRight(..) ) where

import XMonad
import Control.Monad
import qualified XMonad.StackSet as W


-----------------------------------------------------------------------------
-- col master left layout

data ColMasterLeft a = ColMasterLeft {
                   leftMasterColNMaster :: !Int
                 , leftMasterColRatioIncrement :: !Rational
                 , leftMasterColRatio :: !Rational
                 }
            deriving (Show, Read)

instance LayoutClass ColMasterLeft a where
    pureLayout (ColMasterLeft nmaster _ frac) r s = zip ws rs
      where ws = W.integrate s
            rs = colMasterLeftLayout frac r nmaster (length ws)

    pureMessage (ColMasterLeft nmaster delta frac) m =
            msum [fmap resize     (fromMessage m)
                 ,fmap incmastern (fromMessage m)]

      where resize Shrink             = ColMasterLeft nmaster delta (max 0 $ frac-delta)
            resize Expand             = ColMasterLeft nmaster delta (min 1 $ frac+delta)
            incmastern (IncMasterN d) = ColMasterLeft (max 0 (nmaster+d)) delta frac

    description _ = "ColMasterLeft"

colMasterLeftLayout
    :: Rational  -- ^ @frac@, what proportion of the screen to devote to the master area
    -> Rectangle -- ^ @r@, the rectangle representing the screen
    -> Int       -- ^ @nmaster@, the number of windows in the master pane
    -> Int       -- ^ @n@, the total number of windows to tile
    -> [Rectangle]
colMasterLeftLayout f r nmaster n = if n <= nmaster || nmaster == 0
    then splitHorizontally n r
    else splitHorizontally nmaster r1 ++ splitVertically (n-nmaster) r2
  where (r1,r2) = splitHorizontallyBy f r


-----------------------------------------------------------------------------
-- col master right layout

data ColMasterRight a = ColMasterRight {
                   rightMasterColNMaster :: !Int
                 , rightMasterColRatioIncrement :: !Rational
                 , rightMasterColRatio :: !Rational
                 }
            deriving (Show, Read)

instance LayoutClass ColMasterRight a where
    pureLayout (ColMasterRight nmaster _ frac) r s = zip ws rs
      where ws = W.integrate s
            rs = colMasterRightLayout frac r nmaster (length ws)

    pureMessage (ColMasterRight nmaster delta frac) m =
            msum [fmap resize     (fromMessage m)
                 ,fmap incmastern (fromMessage m)]

      where resize Shrink             = ColMasterRight nmaster delta (max 0 $ frac-delta)
            resize Expand             = ColMasterRight nmaster delta (min 1 $ frac+delta)
            incmastern (IncMasterN d) = ColMasterRight (max 0 (nmaster+d)) delta frac

    description _ = "ColMasterRight"

colMasterRightLayout
    :: Rational  -- ^ @frac@, what proportion of the screen to devote to the master area
    -> Rectangle -- ^ @r@, the rectangle representing the screen
    -> Int       -- ^ @nmaster@, the number of windows in the master pane
    -> Int       -- ^ @n@, the total number of windows to tile
    -> [Rectangle]
colMasterRightLayout f r nmaster n = if n <= nmaster || nmaster == 0
    then splitHorizontally n r
    else splitHorizontally nmaster r2 ++ splitVertically (n-nmaster) r1
  where (r1,r2) = splitHorizontallyBy (1-f) r
