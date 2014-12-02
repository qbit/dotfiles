import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Named
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import System.IO

main = do
        status <- spawnPipe myDzenStatus
        xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig 
                   {
                   normalBorderColor = "#666666"
                   ,focusedBorderColor = "#eeeeee"
                   ,terminal = "uxterm"
                   , layoutHook = myLayoutHook
                   ,logHook = myLogHook status
                   }

myLogHook h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }

myLayoutHook = avoidStruts $ smartBorders ( full ||| mtiled ||| tiled )
  where
    full    = named "X" $ Full
    mtiled  = named "M" $ Mirror tiled
    tiled   = named "T" $ Tall 1 (5/100) (2/(1+(toRational(sqrt(5)::Double))))
 
myDzenStatus = "dzen2 -ta 'l'" ++ myDzenStyle
myDzenStyle  = " -h '20' -fg '#777777' -bg '#222222'"
 
myDzenPP  = dzenPP
            { ppCurrent = dzenColor "#3399ff" "" . wrap " " " "
            , ppHidden  = dzenColor "#dddddd" "" . wrap " " " "
            , ppHiddenNoWindows = dzenColor "#777777" "" . wrap " " " "
            , ppUrgent  = dzenColor "#ff0000" "" . wrap " " " "
            , ppSep     = "     "
            , ppLayout  = dzenColor "#aaaaaa" "" . wrap "^ca(1,xdotool key super+space)· " " ·^ca()"
            , ppTitle   = dzenColor "#ffffff" "" 
                          . wrap "^ca(1,xdotool key super+k)^ca(2,xdotool key super+shift+c)"
                                "                          ^ca()^ca()" . shorten 20 . dzenEscape
            }
