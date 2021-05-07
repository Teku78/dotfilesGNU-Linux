import XMonad
import XMonad.Hooks.ManageDocks
import System.IO
import XMonad.Util.EZConfig (additionalKeysP)
import Graphics.X11.ExtraTypes.XF86
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Layout.Spacing

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops

import XMonad.Util.Run(spawnPipe)

import XMonad.Actions.CycleWS -- to move windows

myTerminal   = "alacritty" -- Terminal por defecto
myModMask    = mod4Mask -- Tecla master (windows)
myBorderWidth  = 1 :: Dimension -- Tamaño del borde de las ventanas
myNormalBorderColor = "#FFFFFF" :: String -- Color de la ventana desenfocada
myFocusedBorderColor = "#00FFFF" :: String -- Color de la ventana activa

-- workpace settings
xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
    doubleLts '<' = "<<"
    doubleLts x = [x]

myWorkSpaces :: [String]

myWorkSpaces = clickable . (map xmobarEscape) $ ["\xfcb5 ","web", "media"]
	where
		clickable l = ["<action=xdotool key super+" ++ show (i) ++ "> " ++ ws ++ "</action>" | (i, ws) <- zip [1 .. 9] l]

-- =====================[keys bindings]============================
myKeys :: [(String, X ())]
myKeys = [
	-- Acciones de las ventanas
	("M-r", spawn "xmonad --restart"), 		-- Restart xmonad
	("M-q", kill), 							-- Cerrar ventana
	("M-h", nextScreen),
	("M-l", prevScreen),

	-- Lanzar programas
	("M-m", spawn "rofi -show drun"), 		-- Menu de aplicaciones

	-- Run terminal
	("M-<Return>", spawn myTerminal), 		-- Termianl

	-- Volume
    ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%"),
    ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%"),
    ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle" )

	]

-- ====================[ Layouts ]==================================
myLayoutHook = tiled ||| Mirror tiled ||| Full
	where
		tiled = smartSpacing 3 $ Tall nmaster delta ratio
		nmaster = 1
		ratio = 1/2
		delta = 3/100

myStartupHook :: X ()
myStartupHook = do
	spawn "~/.fehbg"
--	spawn "killall picom; picom --experimental-backends"
-- configuration xmobar

main :: IO ()
main = do
	xmproc <- spawnPipe ("xmobar $HOME/.xmonad/xmobar/xmobarrc")
	--xmproc <- spawnPipe ("xmobar $HOME/.xmonad/xmobar/xmobarrc1")
	xmonad $ ewmh $ defaults {
	manageHook = manageDocks <+> manageHook defaults
	,layoutHook = avoidStruts  $  layoutHook defaults
	-- Esta linea evita que las ventanas se sobrepongan a xmobar
	, handleEventHook = handleEventHook defaults <+> docksEventHook
	,logHook = dynamicLogWithPP xmobarPP {
      ppOutput = hPutStrLn xmproc,
      ppTitle = xmobarColor "green" "" . shorten 50,
	  ppHidden = xmobarColor "#82AAFF" "",
	  ppCurrent = xmobarColor "#c3e88D" "" . wrap "[" " ]",
	  ppVisible = xmobarColor "#c3e88d" ""
      }}
defaults = defaultConfig {
		terminal  		   = myTerminal,
		modMask   		   = myModMask,
		borderWidth  	   = myBorderWidth,
		normalBorderColor  = myNormalBorderColor,
		focusedBorderColor = myFocusedBorderColor,
		layoutHook 		   = myLayoutHook,
		workspaces 		   = myWorkSpaces,
		startupHook        = myStartupHook
    } `additionalKeysP` myKeys
