{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- Colin Desktop Xmonad Config --

-- IMPORTS --
-- Base
import XMonad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Hooks.ManageDocks
-- Layout
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.Layout.Gaps

import Control.Arrow ((***), second)
import qualified XMonad.StackSet as W

import Data.Monoid
import System.Exit

-- Hooks
import XMonad.Hooks.DynamicLog(dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Fonts
myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

-- Terminal
myTerminal      = "alacritty"

-- Clickables
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- ModMask - Defines mod key
myModMask       = mod4Mask

-- Workspaces
myWorkspaces    = ["dev","www","sys","doc","med"]

-- Border
myNormalBorderColor  = "#404040"
myFocusedBorderColor = "#606060"
myBorderWidth   = 2

-- Key Bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    -- launch rofi
    , ((modm,               xK_p     ), spawn "~/.config/rofi/bin/launcher_misc")
    -- launc dmenu
    --, ((modm,               xK_p     ), spawn "dmenu_run")
    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)
     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++
    -- Switch workspaces
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- Mouse Bindings
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]

-- Layouts (mod-shift-space to revert to defaults):
-- Spacing
mySpacing = spacingRaw False -- Only for >1 Window (Disabled)
		       (Border 15 5 5 5) -- Screen Edge Gaps
		       True -- Enable Screen Edge Gaps
		       (Border 15 5 5 5) -- Window Gaps
		       True -- Enable Window Gaps 
			
myLayoutHook = avoidStruts $ mySpacing $ noBorders $ tiled ||| Mirror tiled ||| fullscreenFull tiled ||| leftTiled  ||| simpleTabbed
	where 
	    tiled = ResizableTall nmaster delta ratio []
	    leftTiled = Flip tiled
	    nmaster = 1
	    delta = 3/100
	    ratio = 1/2

-- | Flip a layout, compute its 180 degree rotated form.
newtype Flip l a = Flip (l a) deriving (Show, Read)

instance LayoutClass l a => LayoutClass (Flip l) a where
    runLayout (W.Workspace i (Flip l) ms) r = (map (second flipRect) *** fmap Flip)
                                                `fmap` runLayout (W.Workspace i l ms) (flipRect r)
                                         where screenWidth = fromIntegral $ rect_width r
                                               flipRect (Rectangle rx ry rw rh) = Rectangle (screenWidth - rx - (fromIntegral rw)) ry rw rh
    handleMessage (Flip l) = fmap (fmap Flip) . handleMessage l
    description (Flip l) = "Flip "++ description l

-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
myManageHook = composeAll [ 
      className =? "NVIDIA X Server Settings" --> doFloat
    , resource  =? "desktop_window"  --> doIgnore
    , resource  =? "kdesktop"        --> doIgnore ]

-- Status bars and logging
myLogHook = return ()

-- Startup hook
myStartupHook = do
  spawnOnce "lxsession &"
  spawnOnce "nitrogen --restore &"
  spawnOnce "picom --experimental-backends --backend glx --xrender-sync-fence &"

-- Main Execution
main = do
    -- Xmobar --
	xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc0" 
	xmproc1 <- spawnPipe "xmobar -x 1 $HOME/.config/xmobar/xmobarrc0" 
 	
 	xmonad $ def {
		startupHook = myStartupHook,
		handleEventHook = mempty,
		terminal = myTerminal,
		focusFollowsMouse = myFocusFollowsMouse,
		clickJustFocuses = myClickJustFocuses,
		borderWidth = myBorderWidth,
		modMask = myModMask,
   		workspaces = myWorkspaces,
		normalBorderColor = myNormalBorderColor,
		focusedBorderColor = myFocusedBorderColor,
		keys = myKeys,
		mouseBindings = myMouseBindings,
		layoutHook =  myLayoutHook, 
		manageHook = myManageHook,
		logHook = myLogHook <+> dynamicLogWithPP xmobarPP {
		  ppOutput = \x ->  hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x	
		 ,ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]"   -- Current Workspace in XMobar
		 ,ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""
		 ,ppHiddenNoWindows = xmobarColor "#c792ea" ""
		 ,ppSep = "<fc=#666666> <fn=1>|</fn> </fc>" 		-- Seperator
		}
	}


-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu/rofi",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
