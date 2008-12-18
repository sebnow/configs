-- vim :fdm=marker sw=4 sts=4 ts=4 et ai:

-- Imports {{{
import XMonad
import XMonad.Layout
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.LayoutHints
import XMonad.Layout.ThreeColumns
import XMonad.Hooks.DynamicLog   (PP(..), dynamicLogWithPP, wrap, defaultPP)
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run (spawnPipe)
import XMonad.Hooks.EwmhDesktops
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
myBitmapsDir        = "/home/xilon/.share/icons/dzen"
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
        ((modm , xK_g), spawn ("exec gajim-remote toggle_roster_appearance")),
        ((modm , xK_Down), spawn ("exec mpc stop")),
        ((modm , xK_Up), spawn ("exec mpc toggle")),
        ((modm , xK_Right), spawn ("exec mpc next")),
        ((modm , xK_Left), spawn ("exec mpc prev")),
        ((modm .|. shiftMask, xK_space), spawn ("exec gnome-do"))
    ]
    ++
    -- Remap switching workspaces to M-[asdfzxcv]
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_a, xK_s, xK_d, xK_f, xK_v]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
-- }}}

statusBarCmd= "dzen2 -p -h 16 -ta l -bg '" ++ myNormalBGColor ++ "' -fg '" ++ myNormalFGColor ++ "' -w 768 -sa c -fn '" ++ myFont ++ "' -x 48"

-- Main {{{
--main = do
--    statusBarPipe <- spawnPipe statusBarCmd
main = xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
        modMask = mod4Mask,
        borderWidth = 1,
        terminal = "urxvtc",
        normalBorderColor = myNormalBGColor,
        focusedBorderColor = myFocusedFGColor,
        defaultGaps = [(16,0,0,0)],
        manageHook = manageHook defaultConfig <+> myManageHook,
        layoutHook = onWorkspace "chat" chatLayout globalLayout,
        workspaces = myWorkspaces,
        --logHook = dynamicLogWithPP $ myPP statusBarPipe,
        logHook = myLoghook,
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

-- Dzen Pretty Printer {{{
-- Stolen from Rob [1] and modified
-- [1] http://haskell.org/haskellwiki/Xmonad/Config_archive/Robert_Manea%27s_xmonad.hs
myPP handle = defaultPP {
        ppCurrent = wrap ("^fg(" ++ myFocusedFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p(4)") "^p(4)^fg()^bg()",
        ppUrgent = wrap ("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myUrgentBGColor ++ ")^p(4)") "^p(4)^fg()^bg()",
        ppVisible = wrap ("^fg(" ++ myNormalFGColor ++ ")^bg(" ++ myNormalBGColor ++ ")^p(4)") "^p(4)^fg()^bg()",
        ppSep     = "^fg(" ++ mySeperatorColor ++ ")^r(3x3)^fg()",
        ppLayout  = (\x -> case x of
                    "Tall"          -> " ^i(" ++ myBitmapsDir ++ "/tall.xbm) "
                    "Mirror Tall"   -> " ^i(" ++ myBitmapsDir ++ "/mtall.xbm) "
                    "Full"          -> " ^i(" ++ myBitmapsDir ++ "/full.xbm) "
                    "ThreeCol"      -> " ^i(" ++ myBitmapsDir ++ "/threecol.xbm) "
                    "Hinted Tall"          -> " ^i(" ++ myBitmapsDir ++ "/tall.xbm) "
                    "Hinted Mirror Tall"   -> " ^i(" ++ myBitmapsDir ++ "/mtall.xbm) "
                    "Hinted Full"          -> " ^i(" ++ myBitmapsDir ++ "/full.xbm) "
                    "Hinted ThreeCol"      -> " ^i(" ++ myBitmapsDir ++ "/threecol.xbm) "
                    _               -> " " ++ x ++ " "
                ),
        ppTitle   = wrap ("^fg(" ++ myFocusedFGColor ++ ")") "^fg()" ,
        ppOutput  = hPutStrLn handle
}
-- }}}

-- Needs some arguments, but which?
myLoghook :: X ()
myLoghook = do ewmhDesktopsLogHook
               return ()
