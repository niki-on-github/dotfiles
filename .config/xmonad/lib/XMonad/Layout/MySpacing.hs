{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

-----------------------------------------------------------------------------
--
-- Module      :  XMonad.Layout.MySpacing
-- Copyright   :  (C) --   Brent Yorgey
--                    2018 Yclept Nemo
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  none
-- Stability   :  unstable
-- Portability :  unportable
-- Fork of     :  XMonad.Layout.Spacing
--
-- This extension is intended for wide and ultra wide screen monitors. If you have only one
-- window open on your workspace e.g for writing text with vim, the window is usually stretched
-- across the whole screen. Since the text always starts left-justified, this is very awkward.
-- One possibility solution would be to open an additional window and move the editor to the right
-- side. A more elegant solution is this extension. If there is only one window on current
-- workspace, we shrink it to 16:9 format. Originally I developed this feature in dwm. After
-- moving to Xmonad I missed this feature so I tried to implement it as good as I could
-- (according to my non-existent knowledge of Haskell). Depending on the situation, this behavior
-- can be deactivated or reactivated (toggled) via a shortcut by calling auto16x9SpacingEnabled
-- function. This is very usefull if you want to have a fullscreen VM with an visible bar at the
-- top. Depending on your workflow the default behavior of auto16x9 spacing is set via the first
-- parameter in `auto16x9Spacing Bool ScreenGaps WindowGaps (Layout)`
--
-- NOTE: This is a try and error solution, since I can't program haskell. This
-- solution unfortunately destroys the outer gaps, so I use the normal spacing
-- extension for the outer gaps and this extension for the inner gaps. For an
-- example see $usage.
--
-----------------------------------------------------------------------------

module XMonad.Layout.MySpacing
    ( -- * Usage
      -- $usage
      auto16x9Spacing
    , toggleAuto16x9SpacingEnabled
    , triggerResizeEvent
--    , setInverseSpacing
--    , setScreenSpacing, setScreenSpacingEnabled
--    , setWindowSpacing, setWindowSpacingEnabled
--    , toggleInverseSpacing
--    , toggleWindowSpacingEnabled
--    , setScreenWindowSpacing
--    , incWindowSpacing, incScreenSpacing
--    , decWindowSpacing, decScreenSpacing
--    , incScreenWindowSpacing, decScreenWindowSpacing
--    , borderMap, borderIncrementBy
    ) where

import           XMonad
import           XMonad.StackSet                    as W
import qualified XMonad.Util.Rectangle              as R
import qualified XMonad.Layout.Spacing              as S
import           XMonad.Layout.LayoutModifier
import           XMonad.Actions.MessageFeedback

-- $usage
--
-- > import XMonad.Layout.MySpacing
-- > import XMonad.Layout.NoBorders
-- > import XMonad.Layout.Spacing
--
-- > myKeys = ...
-- >    , ((modm, xK_g), XMonad.Layout.MySpacing.auto16x9SpacingEnabled ) -- toggle auto 16x9 spacing on/off
-- >    ...
--
-- > layoutHook = ... $ smartBorders $ widescreenGaps $ ...
-- >    where
-- >       widescreenGaps = XMonad.Layout.MySpacing.auto16x9Spacing True (Border myGaps myGaps myGaps myGaps) (Border myGaps myGaps myGaps myGaps)
-- >       myGaps = 4
--

-- see XMonad.Layout.Spacing, this is only internal!
data Spacing a = Spacing
    { smartBorder           :: Bool
    , screenBorder          :: S.Border
    , screenBorderEnabled   :: Bool
    , windowBorder          :: S.Border
    , windowBorderEnabled   :: Bool
    } deriving (Show,Read)

-- ugly fix to get inverse_outer gaps like in i3-gaps
-- unfortunately i can't program haskel and had to solve it this way
modifyLayoutInverse (Spacing _b sb _sbe _wb _wbe) wsp lr = do
        let sb1 = borderClampGTZero sb
            lr' = withBorder' sb1 2 lr
            sb2 = toBorder lr' lr
        (wrs,ml) <- runLayout wsp lr'
        let ff (w,wr) (i,ps) = if w `elem` (W.integrate' . W.stack $ wsp)
                               then let wr' = withBorder' sb2 2 wr
                                    in  (i+1,(w,wr'):ps)
                               else let wr' = moveByQuadrant lr wr sb2
                                    in  (i,(w,wr'):ps)
            (c,wrs') = foldr ff (0::Integer,[]) wrs

        return (wrs,ml)

      where
        moveByQuadrant :: Rectangle -> Rectangle -> S.Border -> Rectangle
        moveByQuadrant rr mr@(Rectangle {rect_x = x, rect_y = y}) (S.Border bt bb br bl) =
            let (rcx,rcy) = R.center rr
                (mcx,mcy) = R.center mr
                dx = orderSelect (compare mcx rcx) (bl,0,negate br)
                dy = orderSelect (compare mcy rcy) (bt,0,negate bb)
            in  mr { rect_x = x + fromIntegral dx, rect_y = y + fromIntegral dy }


instance Eq a => LayoutModifier Spacing a where

    modifyLayout (Spacing _b _sb False _wb _wbe) wsp lr =
        runLayout wsp lr
    modifyLayout (Spacing b sb _sbe wb wbe) wsp lr = do
        -- if only one window is visible on workspace we shrink size to 16:9 ratio
        let horizontalGaps = toInteger (div ((rect_width lr) - (div (16 * (rect_height lr)) 9)) 2)
            sb1 = borderClampGTZero (S.Border 0 0 horizontalGaps horizontalGaps)
            lr' = withBorder' sb1 2 lr
            sb2 = toBorder lr' lr
        (wrs,ml) <- runLayout wsp lr'
        let ff (w,wr) (i,ps) = if w `elem` (W.integrate' . W.stack $ wsp)
                               then let wr' = withBorder' sb2 2 wr
                                    in  (i+1,(w,wr'):ps)
                               else let wr' = moveByQuadrant lr wr sb2
                                    in  (i,(w,wr'):ps)
            (c,wrs') = foldr ff (0::Integer,[]) wrs

        -- if more than one window on worcspace: call my ugly fix to get inverse_outer gaps like in i3-gaps
        if c > 1 && b then (modifyLayoutInverse (Spacing b (S.Border 0 0 0 0) False wb wbe) wsp lr) else return $ if b then (wrs,ml) else (wrs',ml)

      where
        moveByQuadrant :: Rectangle -> Rectangle -> S.Border -> Rectangle
        moveByQuadrant rr mr@(Rectangle {rect_x = x, rect_y = y}) (S.Border bt bb br bl) =
            let (rcx,rcy) = R.center rr
                (mcx,mcy) = R.center mr
                dx = orderSelect (compare mcx rcx) (bl,0,negate br)
                dy = orderSelect (compare mcy rcy) (bt,0,negate bb)
            in  mr { rect_x = x + fromIntegral dx, rect_y = y + fromIntegral dy }



    -- This is run after 'modifyLayout' but receives the original stack, not
    -- one possibly modified by the child layout. Does not remove borders from
    -- windows not in the stack, i.e. decorations generated by
    -- 'XMonad.Layout.Decorations'.
    pureModifier (Spacing _b _sb _sbe _wb False) _lr _mst wrs =
        (wrs, Nothing)
    pureModifier (Spacing b _sb _sbe wb _wbe) _lr mst wrs =
        let wb' = borderClampGTZero wb
            ff p@(w,wr) (i,ps) = if w `elem` W.integrate' mst
                                 then let wr' = withBorder' wb' 2 wr
                                      in  (i+1,(w,wr'):ps)
                                 else (i,p:ps)
            (c,wrs') = foldr ff (0::Integer,[]) wrs
        in  if c <= 1 && b
            then (wrs, Nothing)
            else (wrs', Nothing)

    pureMess s@(Spacing b sb sbe wb wbe) m
        | Just (ModifySmartBorder f) <- fromMessage m
        = Just $ s { smartBorder = f b }
        | Just (ModifyScreenBorder f) <- fromMessage m
        = Just $ s { screenBorder = f sb }
        | Just (ModifyScreenBorderEnabled f) <- fromMessage m
        = Just $ s { screenBorderEnabled = f sbe }
        | Just (ModifyWindowBorder f) <- fromMessage m
        = Just $ s { windowBorder = f wb }
        | Just (ModifyWindowBorderEnabled f) <- fromMessage m
        = Just $ s { windowBorderEnabled = f wbe }
        | otherwise
        = Nothing

    modifierDescription Spacing {} =
        "16x9Spacing"

--auto16x9Spacing :: Bool   -- ^ is auto 16x9 default on or off (use toggleAuto16x9SpacingEnabled to switch)
--           -> Border      -- ^ The 'screenBorder'.
--           -> Border      -- ^ The 'windowBorder'.
--           -> l a -> ???
auto16x9Spacing b sb wb a = S.spacingRaw True sb True (S.Border 0 0 0 0) False $ ModifiedLayout (Spacing True (S.Border 0 0 0 0) b wb True) $ a

-- | Messages to alter the state of 'Spacing' using the endomorphic function arguments.
data SpacingModifier
    = ModifySmartBorder (Bool -> Bool)
    | ModifyScreenBorder (S.Border -> S.Border)
    | ModifyScreenBorderEnabled (Bool -> Bool)
    | ModifyWindowBorder (S.Border -> S.Border)
    | ModifyWindowBorderEnabled (Bool -> Bool)
    deriving (Typeable)

instance Message SpacingModifier

-- | Set 'smartBorder' to the given 'Bool'.
-- setSmartSpacing :: Bool -> X ()
-- setSmartSpacing = sendMessage . ModifySmartBorder . const

-- | Set 'screenBorder' to the given 'Border'.
-- setScreenSpacing :: S.Border -> X ()
-- setScreenSpacing = sendMessage . ModifyScreenBorder . const

-- | Set 'screenBorderEnabled' to the given 'Bool'.
-- setScreenSpacingEnabled :: Bool -> X ()
-- setScreenSpacingEnabled = sendMessage . ModifyScreenBorderEnabled . const

-- | Set 'windowBorder' to the given 'Border'.
-- setWindowSpacing :: S.Border -> X ()
-- setWindowSpacing = sendMessage . ModifyWindowBorder . const

-- | Set 'windowBorderEnabled' to the given 'Bool'.
-- setWindowSpacingEnabled :: Bool -> X ()
-- setWindowSpacingEnabled = sendMessage . ModifyWindowBorderEnabled . const

-- | Toggle 'smartBorder'.
-- toggleSmartSpacing :: X ()
-- toggleSmartSpacing = sendMessage $ ModifySmartBorder not

-- keep current value
--pass :: Bool -> Bool
--pass x = x

-- trigger resize event e.g. for Virtual Box guests
triggerResizeEvent :: X()
triggerResizeEvent = do
    sendMessage $ ModifySmartBorder not
    sendMessage $ ModifySmartBorder not

-- | Toggle 'screenBorderEnabled'.
toggleAuto16x9SpacingEnabled :: X ()
toggleAuto16x9SpacingEnabled = sendMessage $ ModifyScreenBorderEnabled not

-- | Toggle 'windowBorderEnabled'.
-- toggleWindowSpacingEnabled :: X ()
-- toggleWindowSpacingEnabled = sendMessage $ ModifyWindowBorderEnabled not

-- | Set all borders to a uniform size; see 'setWindowSpacing' and
-- 'setScreenSpacing'.
-- setScreenWindowSpacing :: Integer -> X ()
-- setScreenWindowSpacing = sendMessages . flip map [ModifyWindowBorder,ModifyScreenBorder]
--                                       . flip id . const . uniformBorder

-- | Increment the borders of 'windowBorder' using 'borderIncrementBy', which
-- preserves border ratios during clamping.
-- incWindowSpacing :: Integer -> X ()
-- incWindowSpacing = sendMessage . ModifyWindowBorder . borderIncrementBy

-- | Increment the borders of 'screenBorder' using 'borderIncrementBy'.
-- incScreenSpacing :: Integer -> X ()
-- incScreenSpacing = sendMessage . ModifyScreenBorder . borderIncrementBy

-- | Inverse of 'incWindowSpacing', equivalent to applying 'negate'.
-- decWindowSpacing :: Integer -> X ()
-- decWindowSpacing = incWindowSpacing . negate

-- | Inverse of 'incScreenSpacing'.
-- decScreenSpacing :: Integer -> X ()
-- decScreenSpacing = incScreenSpacing . negate

-- | Increment both screen and window borders; see 'incWindowSpacing' and
-- 'incScreenSpacing'.
-- incScreenWindowSpacing :: Integer -> X ()
-- incScreenWindowSpacing = sendMessages . flip map [ModifyWindowBorder,ModifyScreenBorder]
--                                       . flip id . borderIncrementBy

-- | Inverse of 'incScreenWindowSpacing'.
-- decScreenWindowSpacing :: Integer -> X ()
-- decScreenWindowSpacing = incScreenWindowSpacing . negate

-- | Construct a uniform 'Border'. That is, having equal individual borders.
-- uniformBorder :: Integer -> S.Border
-- uniformBorder i = S.Border i i i i

-- | Map a function over a 'Border'. That is, over the four individual borders.
borderMap :: (Integer -> Integer) -> S.Border -> S.Border
borderMap f (S.Border t b r l) = S.Border (f t) (f b) (f r) (f l)

-- | Clamp borders to within @[0,Infinity]@.
borderClampGTZero :: S.Border -> S.Border
borderClampGTZero = borderMap (max 0)

-- | Change the border spacing by the provided amount, adjusted so that at
-- least one border field is @>=0@.
-- borderIncrementBy :: Integer -> S.Border -> S.Border
-- borderIncrementBy i (S.Border t b r l) =
--     let bl = [t,b,r,l]
--         o  = maximum bl
--         o' = max i $ negate o
--         [t',b',r',l'] = map (+o') bl
--     in  S.Border t' b' r' l'

-- | Interface to 'XMonad.Util.Rectangle.withBorder'.
withBorder' :: S.Border -> Integer -> Rectangle -> Rectangle
withBorder' (S.Border t b r l) = R.withBorder t b r l

-- | Return the border necessary to derive the second rectangle from the first.
-- Since 'R.withBorder' may scale the borders to stay within rectangle bounds,
-- it is not an invertible operation, i.e. applying a negated border may not
-- return the original rectangle. Use this instead.
toBorder :: Rectangle -> Rectangle -> S.Border
toBorder r1 r2 =
    let R.PointRectangle r1_x1 r1_y1 r1_x2 r1_y2 = R.pixelsToCoordinates r1
        R.PointRectangle r2_x1 r2_y1 r2_x2 r2_y2 = R.pixelsToCoordinates r2
        l = r2_x1 - r1_x1
        r = r1_x2 - r2_x2
        t = r2_y1 - r1_y1
        b = r1_y2 - r2_y2
    in S.Border t b r l

-- | Given an ordering and a three-tuple, return the first tuple entry if 'LT',
-- second if 'EQ' and third if 'GT'.
orderSelect :: Ordering -> (a,a,a) -> a
orderSelect o (lt,eq,gt) = case o of
    LT -> lt
    EQ -> eq
    GT -> gt


