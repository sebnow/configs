import XMonad
import XMonad.Util.Run(spawnPipe)
import qualified Data.Map as Map
import qualified XMonad.StackSet as StackSet

myConfig = defaultConfig {
	terminal = "urxvtc"
	, borderWidth = 1
	, modMask = mod4Mask
	, workspaces = ["www", "chat", "3", "4", "5"]
	, keys = myKeys
}

myKeys conf@(XConfig {XMonad.modMask = modMask}) = Map.fromList $ [
	  ((modMask, xK_Return), spawn $ XMonad.terminal conf)
	, ((modMask, xK_p), spawn "dmenu_run") -- program launcher
	, ((modMask, xK_j), windows StackSet.focusDown)
	, ((modMask, xK_k), windows StackSet.focusUp)
	, ((modMask, xK_h), sendMessage Shrink)
	, ((modMask, xK_l), sendMessage Expand)
	, ((modMask .|. shiftMask, xK_j), windows StackSet.swapDown)
	, ((modMask .|. shiftMask, xK_k), windows StackSet.swapUp)
	, ((modMask .|. shiftMask, xK_x), kill)
	, ((modMask .|. shiftMask, xK_r), restart "xmonad" True)
	]

main = xmonad $ myConfig

