--
-- =============================================================
-- ██╗  ██╗███╗   ███╗ ██████╗ ███╗   ██╗ █████╗ ██████╗
-- ╚██╗██╔╝████╗ ████║██╔═══██╗████╗  ██║██╔══██╗██╔══██╗
--  ╚███╔╝ ██╔████╔██║██║   ██║██╔██╗ ██║███████║██║  ██║
--  ██╔██╗ ██║╚██╔╝██║██║   ██║██║╚██╗██║██╔══██║██║  ██║
-- ██╔╝ ██╗██║ ╚═╝ ██║╚██████╔╝██║ ╚████║██║  ██║██████╔╝
-- ╚═╝  ╚═╝╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚═╝  ╚═╝╚═════╝
-- =============================================================
--
import XMonad
import System.IO (hPutStrLn) -- for xmobar output
import System.Exit
import Data.Monoid
import qualified Data.Map as M
import Data.Ratio -- %

--import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.ManageDocks

import qualified XMonad.StackSet as W

--import XMonad.Layout.IndependentScreens
--import XMonad.Actions.Warp (warpToScreen)

-- Status bar
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageHelpers (composeOne, isFullscreen, isDialog, doFullFloat, doCenterFloat)

import XMonad.Util.Run -- For spawnPipe and hPutStrLn
import XMonad.Util.SpawnOnce
import XMonad.Util.Cursor
import XMonad.Util.EZConfig (additionalKeysP)

import XMonad.Config.Desktop

-- windows actions
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Actions.WithAll (sinkAll)
import XMonad.Actions.Promote
import XMonad.Actions.CycleWS -- to move windows
import XMonad.Actions.CycleWindows
import qualified XMonad.Actions.CycleWS as CWs -- girar las ventanas en la misma pantalla
------------------------------------------------------------------------------------------
-- Layouts
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.ResizableTile
import XMonad.Layout.TwoPane
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle as MT (Toggle(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL, MIRROR, NOBORDERS))

------------------------------------------------------------------------------------------
myTerminal           = "alacritty" :: String     -- Terminal
myModMask            = mod4Mask    :: KeyMask    -- leader key (windows)
myBorderWidth        = 2           :: Dimension  -- Border size
myNormalBorderColor  = "#3e4564"   :: String     -- Border color of unfocus window
myFocusedBorderColor = "#bfc9f4"   :: String     -- Border color of focus window
myFocusFollowsMouse  = True        :: Bool
myClickJustFocuses   = False       :: Bool

-- Start Up --------------------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do	
	spawn "~/.fehbg" -- Set background
    -- spawn "killall picom;sleep 2; picom --experimental-backends"
	setDefaultCursor xC_left_ptr -- Fix cursor theme
	spawn "xrandr --output HDMI1 --primary --auto --output eDP1 --auto --below HDMI1"

-- Rule of the windows --------------------------------------------------------------------------- 
myManageHook = composeOne [
    transience
    , isDialog  -?> doCenterFloat
    ] <+> composeAll [
    className   =? "tk"             --> doFloat
    , className =? "Pcmanfm"        <&&> resource =? "Moving files" --> doCenterFloat
    , className =? "firefox"        <&&> resource =? "Toolkit" --> doFloat
    , resource  =? "dialog"         --> doCenterFloat
    , className =? "feh"            --> doCenterFloat
    , className =? "Gcolor3"        --> doFloat
    , className =? "Lxappearance"   --> doCenterFloat
    , resource  =? "desktop_window" --> doIgnore
    , isFullscreen --> doFullFloat
    ]

-- Workpace --------------------------------------------------------------------------------------

myWorkSpaces :: [String]

-- myWorkSpaces = map show [1..5 :: Int] -- Another way to define workpaces
myWorkSpaces = ["1", "2"]
-- Icons for identify the status of the workpace
currentWorkspace  :: String -> String
occupiedWorkspace :: String -> String
screen2Workspace  :: String -> String

currentWorkspace  _ = "<fn=1>\xF0BAF</fn>" 
occupiedWorkspace _ = "<fn=1>\xF02A0</fn>"
screen2Workspace  _ = "<fn=1>\xF02A0</fn>"

-- this show the number of windows for each workspace.
windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Layouts --------------------------------------------------------------------------------------

-- define the size of gaps
mySpacing = spacingRaw False             -- False=Apply even when single window
                       (Border 8 8 8 8)  -- Sreen border size top bot right lefth
                       True              -- Enable sreen boder
                       (Border 8 8 8 8)  -- Window Border size
                       True              -- Enable window borders

myLayoutHook = avoidStruts $ tall ||| grid ||| full
	where
		
        tall =  renamed [Replace "\xfa6d "] --舘
                 $ mySpacing
                 $ Tall nmaster delta ratio

        grid = renamed [Replace "\xfc56 "] --ﱖ
               $ mySpacing
               -- $ limitWindows 5
               $ Grid

        full = renamed [Replace "\xf630 "] --
               $ noBorders (Full)

        nmaster = 1
        delta = 3/100
        ratio = 1/2

-- Xmobar ----------------------------------------------------------------------------------------
myLogHook one two = xmobarPP {
    ppOutput = \x -> hPutStrLn one x >> hPutStrLn two x

   -- , ppCurrent         = xmobarColor "#050608,#bfc9f4" "" . wrap (xmobarColor"#050608,#bfc9f4" "" "\xe0b0")(xmobarColor"#bfc9f4" "" "\xe0b0") . currentWorkspace
    , ppCurrent         = xmobarColor "#ffc857" "" . currentWorkspace
    , ppVisible         = xmobarColor "#ff1767" "" . screen2Workspace -- . wrap ("")("") -- . \s -> " \xf878 "
    , ppHiddenNoWindows = xmobarColor "#223045" "" . occupiedWorkspace
    , ppHidden          = xmobarColor "#29c1dc" "" . occupiedWorkspace
    , ppTitle           = xmobarColor "#bfc9f4" "" . shorten 55
    --, ppLayout          = xmobarColor "#29c1dc" ""
    , ppSep             = "<fc=#5d647e> · </fc>"
    , ppWsSep           = " "
    , ppUrgent          = xmobarColor "#C45500" "" . wrap "" "!"
    , ppExtras          =  [windowCount]
    --, ppOrder           = \(ws : l : t : ex) -> [ws,l] ++ex++ [t] -- default
    , ppOrder           = \(ws :_:t:ex) -> [ws]++ex++[t] -- don't show the Layouts state

}

-- Keybinding -------------------------------------------------------------------------------------
myKeys = [
	
	-- : Xmonad
    ("M-q"  , kill)									-- Close current window
	, ("M-r"    , spawn "xmonad --restart")			-- Restart xmonad 

	, ("M-<Tab>"  , CWs.nextScreen)                 -- Move between monitor
--    , ("M-l", CWs.prevScreen)                     -- mover el cursor de regreso a otro monitor
    , ("M-S-l", CWs.shiftToNext)                    -- Move the focused window to the next workspace
    , ("M-S-h", CWs.shiftToPrev)
    , ("M-h"  , prevWS)                             -- Move to the prev workspace
    , ("M-l"  , nextWS)                             -- Move to the next workspace
    , ("M-C-f", sinkAll)                            -- Reordenar la ventanas que esten flotando
    , ("M-w"  , sendMessage Shrink)
    , ("M-e"  , sendMessage Expand)
    , ("M-S-j", windows W.swapUp)                   -- Rotar la ventana a la derecha en el mismo workspace
    , ("M-t"  , sendMessage ToggleStruts)           -- Put the windows in full screen
    , ("M-f"  , windows W.focusMaster)              -- Move to the master window
    , ("M-j"  , windows W.focusDown)
    , ("M-n"  , promote)                            -- Move focused window to master.
    , ("M-a", sendMessage MirrorExpand)
    , ("M-u", sendMessage $ JumpToLayout "full")

    -- Programs
    , ("M-m", sendMessage ShrinkSlave)
    , ("M-S-p", spawn "flameshot screen -n 0 -d 1000 -p ~/Pictures/Screenshot/")       -- Take a screenshot of the current window / install flameshot
	, ("M-s", spawn "flameshot gui")
    , ("M-p"  , spawn "rofi -show drun")            -- Lauch then menu programs / install rofi
    , ("M-<Return>", spawn myTerminal)              -- Lauch terminal
    -- , ("M-S-s", spawn "firefox --private-window")   -- ;p

    -- Media controls

     -- Volume
    , ("<XF86AudioLowerVolume>", spawn "pamixer --decrease  2")
    , ("<XF86AudioRaiseVolume>", spawn "pamixer --increase 2")
    , ("<XF86AudioMute>", spawn "pamixer --toggle-mute")
    -- Music
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86AudioPrev>", spawn "playerctl previous")


    -- Brightness
    , ("<XF86MonBrightnessUp>", spawn "brightnessctl set +10%")
    , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 10%-")
    ]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ (( modMask, button2), (\w -> focus w >> mouseResizeWindow w
                                           >> windows W.shiftMaster))
    , (( modMask, button1), (\w -> focus w >> mouseMoveWindow w
                                           >> windows W.shiftMaster))
    ]

-- Main ------------------------------------------------------------------------------------------

main :: IO ()
main = do
    laptopBar      <- spawnPipe ("xmobar -x 0 $HOME/.xmonad/xmobar/xmobarLaptop")
    extrMonitorBar <- spawnPipe ("xmobar -x 1 $HOME/.xmonad/xmobar/xmobarMonitor")

    xmonad . docks . ewmh . ewmhFullscreen $ def {
    terminal                = myTerminal
    , modMask               = myModMask
    , borderWidth           = myBorderWidth
    , normalBorderColor     = myNormalBorderColor
    , focusedBorderColor    = myFocusedBorderColor
    , focusFollowsMouse     = myFocusFollowsMouse
    , clickJustFocuses      = myClickJustFocuses
    , mouseBindings         = myMouseBindings
    , layoutHook            = myLayoutHook
    , workspaces            = myWorkSpaces
    , startupHook           = myStartupHook
    , manageHook            = myManageHook <+> manageDocks
    , logHook               = (dynamicLogWithPP $ myLogHook laptopBar extrMonitorBar) >> updatePointer(0.5,0.5)(0.5, 0.5)

    } `additionalKeysP` myKeys

