{-# LANGUAGE QuasiQuotes #-}

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.Minimize
import XMonad.Layout.Minimize
import XMonad.Layout.Gaps
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Accordion
import XMonad.Layout.Magnifier
import XMonad.Layout.ResizableTile
import XMonad.Util.EZConfig
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.XSelection
import System.IO
import Data.Monoid
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.OpenBSD.Process ( pledge )    

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
      name     <- getName w
      Just idx <- fmap (W.findTag w) $ gets windowset
      safeSpawn "notify-send" [show name, "workspace " ++ idx]

main :: IO()
main = do
  _ <- pledge (Just "stdio rpath wpath cpath proc exec unix") Nothing
  status <- spawnPipe myXmoStatus
  xmonad $ ewmh $ withUrgencyHook LibNotifyUrgencyHook
    $ defaultConfig
    {
      normalBorderColor = "#666666"
    , focusedBorderColor = "darkgrey"
    , focusFollowsMouse  = False
    --, terminal = "urxvtc"
    , terminal = "xterm"
    , workspaces = myWorkspaces
    , startupHook = myStartupHook
    , layoutHook = myLayoutHook
    , logHook = myLogHook status
    , keys = \c -> myKeys c `M.union` XMonad.keys defaultConfig c
    , manageHook = manageDocks <+> myManageHook
      <+> manageHook defaultConfig
    }
    `removeKeysP` ["M-p"] -- don't clober emacs.

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ myXmoPP {ppOutput = hPutStrLn h}

myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys (XConfig {XMonad.modMask = modMask, XMonad.terminal = term}) = M.fromList [
  ((modMask .|. shiftMask  , xK_Right ), shiftToNext)
  ,((modMask .|. shiftMask  , xK_Left  ), shiftToPrev)
  ,((modMask, xK_r)         , spawn "rofi -show run")
  ,((modMask, xK_i)         , spawn "~/.screenlayout/internal.sh")
  ,((modMask, xK_e)         , spawn "~/.screenlayout/external.sh")
  ,((modMask .|. shiftMask  , xK_p), spawn "mpc toggle")
  ,((modMask, xK_p)         , spawn "mpc prev")
  ,((modMask, xK_n)         , spawn "mpc next")
  ]

xmobarEscape :: [Char] -> [Char]
xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . (map xmobarEscape) $ ["emacs","browser","irc","mail","5","6","7","8","console"]
  where clickable l = [ "<action=xdotool key alt+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                        (i,ws) <- zip [1..9] l,
                        let n = i ]

myLayoutHook = avoidStruts $ smartBorders ( tiled ||| mtiled ||| mtl ||| mwl ||| accordion )
  where
    --full     = named "X" $ spacing 3 $ Full
    tiled    = named "T" $ spacing 3 $ Tall 1 (5/100) (2/(1+(toRational(sqrt(5)::Double))))
    mtiled  = named "M" $ spacing 3 $ Mirror tiled
    accordion = named "A" $ spacing 3 $ Accordion
    tallLayout = named "Tall" $ spacing 3 $ ResizableTall nmaster delta ratio slaves
      where nmaster = 1
            delta = 3/100
            ratio = 1/2
            slaves = []
    wideLayout = named "Wide" $ spacing 3 $ Mirror $ tallLayout
    mtl = named "MT" $ magnifiercz' magnifyRatio $ tallLayout
      where magnifyRatio = 1.5
    mwl = named "MW Wide" $ spacing 3 $ magnifiercz' magnifyRatio $ wideLayout
      where magnifyRatio = 1.5

myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
  [
    className =? "MPlayer"           --> doFloat
  , className =? "VLC"               --> doFloat
  , className =? "Pinentry-gtk-2"    --> doFloat
  , className =? "Pinentry-gnome3"    --> doFloat
  , className =? "XCalc"             --> doFloat
  , className =? "chromium-browser"  --> doF (W.shift (myWorkspaces !! 1)) -- send to ws 2
  , className =? "Firefox"           --> doF (W.shift (myWorkspaces !! 1)) -- send to ws 2
  , title =? "Mail"                  --> doF (W.shift (myWorkspaces !! 3))
  , className =? "XConsole"          --> doF (W.shift (myWorkspaces !! 8))
  ]

myStartupHook :: X ()
myStartupHook = do
  --spawnOnce "urxvtc -name Mail"
  spawnOnce "xterm -name Mail"
  --spawnOnce "/home/qbit/bin/browser"

myXmoStatus :: String
myXmoStatus = "xmobar"

myXmoPP :: PP
myXmoPP = xmobarPP
  {
    ppCurrent = xmobarColor "#443740" "" . wrap " " " "
  , ppHidden  = xmobarColor "#ffffff" "" . wrap " " " "
  , ppUrgent  = xmobarColor "#ff0000" "" . wrap " " " "
  , ppSep     = "     "
  , ppLayout  = xmobarColor "#ffffff" "" . wrap "|" "|"
  , ppTitle   = xmobarColor "#ffffff" "" . shorten 20
  }
