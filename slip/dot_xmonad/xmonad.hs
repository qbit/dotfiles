{-# LANGUAGE QuasiQuotes #-}

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Gaps
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import System.IO
import Data.Monoid
import qualified XMonad.StackSet as W
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
  xmonad $ withUrgencyHook LibNotifyUrgencyHook
             $ defaultConfig
             {
               normalBorderColor = "#666666"
             , focusedBorderColor = "darkgrey"
             , terminal = "urxvtc"
             , workspaces = myWorkspaces
             , layoutHook = myLayoutHook
             , logHook = myLogHook status
             , manageHook = manageDocks <+> myManageHook
                            <+> manageHook defaultConfig
             }
             `removeKeysP` ["M-p"] -- don't clober emacs.
             `additionalKeysP` myKeys

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ myXmoPP {ppOutput = hPutStrLn h}

myKeys :: [([Char], X ())]
myKeys =
    [
     ("M-r", spawn "rofi -show run")
    , ("M-z", spawn "xmonad --recompile && xmonad --restart")
    , ("M-i", spawn "~/.screenlayout/internal.sh")
    , ("M-e", spawn "~/.screenlayout/external.sh")
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

myLayoutHook = avoidStruts $ smartBorders ( tiled ||| ptiled ||| mtiled ||| full )
    where
      full     = named "X" $ Full
      tiled    = named "T" $ spacing 3 $ Tall 1 (5/100) (2/(1+(toRational(sqrt(5)::Double))))
      mtiled  = named "M" $ spacing 3 $ Mirror tiled
      ptiled   = named "pT" $ spacing 3 $ gaps [(U,60), (L,60), (R,60), (D,60)] $ Tall 1 (5/100) (2/(1+(toRational(sqrt(5)::Double))))

myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
               [
                className =? "MPlayer"            --> doFloat
               , className =? "XCalc"             --> doFloat
               , className =? "chromium-browser"  --> doF (W.shift (myWorkspaces !! 1)) -- send to ws 2
               , className =? "Firefox"           --> doF (W.shift (myWorkspaces !! 1)) -- send to ws 2
               , className =? "XConsole"          --> doF (W.shift (myWorkspaces !! 8))
               ]

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
