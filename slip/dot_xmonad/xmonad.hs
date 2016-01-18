{-# LANGUAGE QuasiQuotes #-}

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import System.IO
import Data.Monoid
import qualified XMonad.StackSet as W

-- until the pledge interface is in ghc
import Foreign
import Foreign.C
import System.Posix.Process.Internals
import System.Posix.Internals ( withFilePath )

pledge :: String -> [FilePath] -> IO ()
pledge promises paths =
  withCString promises $ \cproms ->
  withMany withFilePath paths $ \cstrs ->
  withArray0 nullPtr cstrs $ \paths_arr ->
  -- not yet:
  -- throwErrnoIfMinus1_ "pledge" (c_pledge cproms paths_arr)
  throwErrnoIfMinus1_ "pledge" (c_pledge cproms nullPtr)

foreign import ccall unsafe "pledge"
  c_pledge :: CString -> Ptr CString -> IO CInt
-- pledge              

main :: IO()
main = do
  -- need to look to see what is actually used here :P
  _ <- pledge "stdio rpath wpath cpath proc exec unix" []
  status <- spawnPipe myXmoStatus
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
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

myLayoutHook = avoidStruts $ smartBorders ( tiled ||| ptiled |||  full )
    where
      full     = named "X" $ Full
      tiled    = named "T" $ spacing 3 $ Tall 1 (5/100) (2/(1+(toRational(sqrt(5)::Double))))
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
          , ppTitle   = xmobarColor "#ffffff" "" . shorten 25
          }
