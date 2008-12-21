-- vim :fdm=marker sw=4 sts=4 ts=4 et ai:

-- Imports {{{
import XMonad
import XMonad.Layout
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.LayoutHints
import XMonad.Layout.ThreeColumns
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run (spawnPipe)
import qualified XMonad.StackSet as W
import qualified Data.Map as M

import System.IO (hPutStrLn)
-- }}}

-- Control Center {{{
-- Colour scheme {{{
myNormalBGColor     = "#2e3436"
myFocusedBGColor    = "#414141"
myNormalFGColor     = "#babdb6"
myFocusedFGColor    = "#73d216"
myUrgentFGColor     = "#f57900"
myUrgentBGColor     = myNormalBGColor
mySeperatorColor    = "#2e3436"
-- }}}
-- Icon packs can be found here:
-- http://robm.selfip.net/wiki.sh/-main/DzenIconPacks
myFont              = "-*-terminus-medium-*-*-*-12-*-*-*-*-*-iso8859-1"
-- }}}

-- Workspaces {{{
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["general", "internet", "chat", "code"] ++ map show [5..9 :: Int]
-- }}}

-- Keybindings {{{
myKeys conf@(XConfig {modMask = modm}) = M.fromList $
    [
        ((modm , xK_p), spawn ("exec `dmenu_path | dmenu -fn '" ++ myFont ++ "' -nb '" ++ myNormalBGColor ++ "' -nf '" ++ myNormalFGColor ++ "' -sb '" ++ myFocusedBGColor ++ "' -sf '" ++ myFocusedFGColor ++ "'`")),
        ((modm , xK_Down), spawn ("exec mpc stop")),
        ((modm , xK_Up), spawn ("exec mpc toggle")),
        ((modm , xK_Right), spawn ("exec mpc next")),
        ((modm , xK_Left), spawn ("exec mpc prev"))
    ]
    ++
    -- Remap switching workspaces to M-[asdfzxcv]
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_a, xK_s, xK_d, xK_f, xK_v]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
-- }}}

-- Main {{{
main = xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
        modMask = mod4Mask,
        borderWidth = 1,
        terminal = "urxvtc",
        normalBorderColor = myNormalBGColor,
        focusedBorderColor = myFocusedFGColor,
        manageHook = manageHook defaultConfig <+> myManageHook,
        layoutHook = onWorkspace "chat" chatLayout globalLayout,
        workspaces = myWorkspaces,
        keys = \c -> myKeys c `M.union` keys defaultConfig c
    }
    where
        globalLayout = layoutHints (tiled) ||| layoutHints (noBorders Full) ||| layoutHints (Mirror tiled) ||| layoutHints (Tall 1 (3/100) (1/2))
        chatLayout = layoutHints (noBorders Full) ||| layoutHints (Tall 1 (3/100) (1/2))
        tiled = ThreeCol 1 (3/100) (1/2)
-- }}}

-- Window rules (floating, tagging, etc) {{{
myManageHook = composeAll [
        className   =? "Firefox-bin"        --> doF(W.shift "internet"),
        className   =? "Minefield"          --> doF(W.shift "internet"),
        className   =? "Gajim.py"           --> doF(W.shift "chat"),

        title       =? "Gajim"              --> doFloat,
        className   =? "stalonetray"        --> doIgnore,
        className   =? "Gnome-panel"        --> doIgnore,
        className   =? "Do"                 --> doIgnore
    ]
-- }}}

