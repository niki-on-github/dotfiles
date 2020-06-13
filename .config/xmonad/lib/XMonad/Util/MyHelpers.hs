-----------------------------------------------------------------------------
--
-- Module      :  XMonad.Util.MyHelpers
-- Copyright   :  (c) none
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  none
-- Stability   :  unstable
-- Portability :  unportable
--
-- TODO: I hate the haskell IO system -> How can I get around this?
--
-----------------------------------------------------------------------------

module XMonad.Util.MyHelpers ( 
    getScreenResolution,
    getScreenRatio, 
    ScreenResolution(..) 
    ) where

import XMonad
import Graphics.X11.Xinerama

data ScreenResolution = ScreenResolution { 
    xRes :: Int
    , yRes :: Int
}

getScreenResolution :: String -> Int -> IO ScreenResolution
getScreenResolution d n = do
    dpy <- openDisplay d
    r <- liftIO $ Graphics.X11.Xinerama.getScreenInfo dpy
    closeDisplay dpy
    return $ ScreenResolution
        { xRes = fromIntegral $ rect_width $ r !! n
        , yRes = fromIntegral $ rect_height $ r !! n
        }

getScreenRatio :: String -> Int -> IO Float
getScreenRatio d n = do
    dpy <- openDisplay d
    r <- liftIO $ Graphics.X11.Xinerama.getScreenInfo dpy
    closeDisplay dpy
    let x = fromIntegral $ rect_width $ r !! n
    let y = fromIntegral $ rect_height $ r !! n
    return $ (1.0 * x / (1.0 * y))

