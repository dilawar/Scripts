import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.XPropManage
import XMonad.Hooks.FadeInactive
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import System.IO
import qualified XMonad.StackSet as W
import XMonad.StackSet(stack)
import XMonad.Actions.TagWindows
 
main = do
  xmobar <- spawnPipe "xmobar ~/Scripts/xmobarrc"
  workspaceBarPipe <- spawnPipe myStatusBar
--  spawn myBattery
--  spawn myConkyBar
  xmonad $ defaultConfig
    { manageHook = manageDocks <+> manageHook defaultConfig <+> myManageHook
    , layoutHook = avoidStruts $ layoutHook defaultConfig
    , terminal = "gnome-terminal"
    , workspaces = myWorkspaces
    , startupHook = myStartupHook
    , logHook = myLogHook xmobar 
    , modMask = mod4Mask
    } `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
    , ((mod4Mask, xK_t), spawn "xterm")
    , ((0, xK_Print), spawn "scrot -e 'mv $f ~/Pictures/shots/'")
    , ((mod4Mask, xK_p), runOrRaisePrompt defaultXPConfig)
    , ((mod4Mask, xK_m), focusUpTaggedGlobal "mail")
    , ((mod4Mask, xK_b), focusUpTaggedGlobal "web")
    , ((mod4Mask, xK_d), focusUpTaggedGlobal "dev")
    , ((0, 0x1008FF11), spawn "amixer set Master playback 3-")
    , ((0, 0x1008FF12), spawn "amixer set Master playback 0")
    , ((0, 0x1008FF13), spawn "amixer set Master playback 3+")
    , ((controlMask .|. mod1Mask, xK_t), spawn "mpc toggle")
    , ((controlMask .|. mod1Mask, xK_p), spawn "mpc prev")
    , ((controlMask .|. mod1Mask, xK_n), spawn "mpc next")
    , ((controlMask .|. mod1Mask, xK_d), spawn "~/Scripts/manage_mpc.sh -d")
    , ((controlMask .|. mod1Mask, xK_a), spawn "~/Scripts/manage_mpc.sh -a")
    ]
 
ws_term = "1:control"
ws_web = "2:read"
ws_dev = "3:dev"
ws_comms = "4:write"
ws_vid = "5:web"
ws_mus = "6:serv"
ws_stats = "7:mult"
ws_vms = "8:vms"
ws_9 = "9:others"

myStatusBar :: String
myStatusBar = "dzen2 -a -fn '-*-terminus-bold-r-normal-*-12-*-*-*-*-*-*-*'  -bg '#000000' -fg '#444444' -sa c -x 0 -y 0 -e '' -ta l -xs 1"

myBattery :: String
myBattery = "~/Scripts/battery.sh"

myConkyBar :: String
myConkyBar = "sleep 1 && conky -c ~/Scripts/conky_time | dzen2 -fn '-*-terminus-bold-r-normal-*-12-*-*-*-*-*-*-*' -bg black -fg green -h 10 -sa c -x 1000 -y 0 -e '' -ta r -xs 1"
 
myLogHook :: Handle -> X ()
myLogHook xmobar = dynamicLogWithPP $ defaultPP {
                     ppOutput = hPutStrLn xmobar
                   , ppTitle = xmobarColor "white" "" . shorten 110
                   , ppCurrent = xmobarColor "white" "black" . pad
                   , ppHidden = pad
                   , ppHiddenNoWindows = \w -> xmobarColor "#444" "" (" " ++ w ++ " ")
                   , ppSep = xmobarColor "#555" "" " / "
                   , ppWsSep = ""
                   , ppLayout = \x -> case x of
                                        "Tall" -> "T"
                                        "Mirror Tall" -> "M"
                                        "Full" -> "F"
                                        _ -> "?"
                   }

{-

myLogHook h = dynamicLogWithPP $ defaultPP {   
    ppCurrent	= dzenColor "black" "green" . pad
	, ppVisible	= dzenColor "black" "lightgreen" . pad
	, ppHidden	= dzenColor "#cccccc" "" . pad
	, ppHiddenNoWindows = dzenColor "#444444"  "" . pad
	, ppUrgent	= dzenColor "" "red"
	, ppWsSep    = " "
	, ppSep      = " | "
	, ppTitle    = (" " ++) . dzenColor "green" "" . dzenEscape
	, ppOutput   = hPutStrLn h
 
      }
-}
myWorkspaces = [ ws_term,ws_web,ws_dev,ws_comms,ws_vid,ws_mus,ws_stats,ws_vms,ws_9 ]
 
myManageHook = ( composeAll
    [ appName =? "mutt" --> doShift ws_comms
    , appName =? "irssi" --> doShift ws_comms
    , className =? "firefox" --> doShift ws_web
    , className =? "virtualbox" --> doShift ws_vms
    , appName =? "htop" --> doShift ws_stats
    , appName =? "feh" --> doShift ws_stats
    , appName =? "guake" --> doFloat
    , appName =? "dev1" --> doShift ws_dev
    , appName =? "dev2" --> doShift ws_dev
    ] ) <+> xPropManageHook xPropMatches
 
myStartupHook = do
  --spawn "urxvt -title mutt -name mutt -e mutt -y"
  --spawn "firefox"
  spawn "xterm"
  spawn "mpd"
  --spawn "urxvt -title dev1 -name dev1"
  --spawn "urxvt -title dev2 -name dev2"
  --spawn "virtualbox"
 
xPropMatches :: [XPropMatch]
xPropMatches = [ ([(wM_CLASS, any ("mutt"==))], pmX(addTag "mail"))
               , ([(wM_CLASS, any ("rhythmbox"==))], pmX(addTag "mus"))
               , ([(wM_CLASS, any ("centerim"==))], pmX(addTag "chat"))
               , ([(wM_CLASS, any ("firefox"==))], pmX(addTag "web"))
               , ([(wM_CLASS, any ("eclipse"==))], pmX(addTag "dev"))
               ]
