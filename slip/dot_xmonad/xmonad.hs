import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import System.IO

import qualified XMonad.StackSet as W 

main = do
        status <- spawnPipe myXmoStatus
        xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig 
                   {
                   normalBorderColor = "#666666"
                   ,focusedBorderColor = "#eeeeee"
                   ,terminal = "urxvtc"
                   ,workspaces = myWorkspaces
                   ,layoutHook = myLayoutHook
                   ,logHook = myLogHook status
                   , manageHook = manageDocks <+> myManageHook
                            <+> manageHook defaultConfig
                   }
                   `removeKeysP` ["M-p"] -- don't clober emacs.
                   `additionalKeysP` myKeys

myLogHook h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }

myKeys = [
  ("M-d", spawn "dmenu_run")
  ]

myWorkspaces            :: [String]
myWorkspaces            = clickable . (map dzenEscape) $ ["1","2","3","4","5","6","7","8","9"]
   where clickable l     = [ "^ca(1,xdotool key alt+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                            (i,ws) <- zip [1..] l,
                            let n = i ]

myLayoutHook = avoidStruts $ smartBorders ( tiled ||| mtiled ||| full )
  where
    full    = named "X" $ Full
    mtiled  = named "M" $ Mirror tiled
    tiled   = named "T" $ Tall 1 (5/100) (2/(1+(toRational(sqrt(5)::Double))))

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "XCalc"          --> doFloat
    , className =? "Chrome"       --> doF (W.shift (myWorkspaces !! 1)) -- send to ws 2
    , className =? "Gimp"           --> doF (W.shift (myWorkspaces !! 3)) -- send to ws 4
    ]    

myXmoStatus = "~/.cabal/bin/xmobar"
 
myDzenStatus = "dzen2 -ta 'l'" ++ myDzenStyle
myDzenStyle  = " -h '20' -fg '#777777' -bg '#222222'"
 
myDzenPP  = dzenPP
            { ppCurrent = dzenColor "#3399ff" "" . wrap " " " "
            , ppHidden  = dzenColor "#dddddd" "" . wrap " " " "
            , ppHiddenNoWindows = dzenColor "#777777" "" . wrap " " " "
            , ppUrgent  = dzenColor "#ff0000" "" . wrap " " " "
            , ppSep     = "     "
            , ppLayout  = dzenColor "#aaaaaa" "" . wrap "^ca(1,xdotool key alt+space)· " " ·^ca()"
            , ppTitle   = dzenColor "#ffffff" "" 
                          . wrap "^ca(1,xdotool key alt+k)^ca(2,xdotool key alt+shift+c)"
                                "                          ^ca()^ca()" . shorten 20 . dzenEscape
            }
