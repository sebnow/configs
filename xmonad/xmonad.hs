import qualified Data.Map as M
import System.Directory (getAppUserDataDirectory)
import System.FilePath((</>))
import XMonad
import XMonad.Hooks.DynamicLog hiding (dzen)
import XMonad.Hooks.UrgencyHook (withUrgencyHook, NoUrgencyHook(..))
import XMonad.Layout.Decoration (Theme(..), defaultTheme)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.ManageHook
import XMonad.Util.Cursor
import XMonad.Util.Themes (ThemeInfo(..))

-- Whether to hide empty workspaces in DynamicLog hook
hiddenEnabled :: Bool
hiddenEnabled = False

myWorkspaces         = ["1:main", "2:web", "3:chat"] ++ map show [4 .. 9]
(wsMain : wsWeb : wsChat : _) = myWorkspaces

myConfig = defaultConfig
    { terminal           = "urxvtc"
    , normalBorderColor  = inactiveBorderColor myTheme
    , focusedBorderColor = activeBorderColor myTheme
    , borderWidth        = 1
    , modMask            = mod4Mask
    , workspaces         = myWorkspaces
    , keys               = \c -> myKeys c `M.union` keys defaultConfig c
    , startupHook        = setDefaultCursor xC_left_ptr
    , layoutHook         = smartBorders $ layoutHook defaultConfig
    , manageHook         = myManageHook <+> manageHook defaultConfig
    }

trayerWidth :: (Integral a) => a
trayerWidth = 48

-- Key bindings {{{
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList
    [ ((modMask, xK_p), spawn $ themedDmenu myTheme) -- program launcher
    ]

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modMask} = (modMask, xK_b)
-- }}}

myManageHook = composeAll
    [ className =? "Chromium" --> doShift wsWeb
    , className =? "Pidgin" --> doShift wsChat
    ]

-- Themes {{{
tangoTheme :: ThemeInfo
tangoTheme = TI
    { themeName        = "Tango!"
    , themeAuthor      = "Sebastian Nowicki"
    , themeDescription = "Tango Project color scheme"
    , theme = defaultTheme
        { activeColor         = "#3465a4"
        , activeTextColor     = "#eeeeec"
        , activeBorderColor   = "#204a87"
        , inactiveColor       = "#2e3436"
        , inactiveTextColor   = "#d3d7cf"
        , inactiveBorderColor = "#2e3436"
        , urgentColor         = "#2e3436"
        , urgentTextColor     = "#f57900"
        , urgentBorderColor   = "#ce5c00"
        , decoHeight          = 16
        }
    }

myTheme :: Theme
myTheme = (theme tangoTheme)
    { fontName = "-*-terminus-medium-*-*-*-*-*-*-*-*-*-*-*"
    }
-- }}}

-- Return dmenu cmd specifying an XMonad theme
themedDmenu :: Theme -> String
themedDmenu t = unwords
    [ "dmenu_run"
    , "-fn", quote $ fontName t
    , "-nb", quote $ inactiveColor t
    , "-nf", quote $ inactiveTextColor t
    , "-sb", quote $ activeColor t
    , "-sf", quote $ activeTextColor t
    ]

data Alignment = AlignLeft | AlignCenter | AlignRight

dzenCmd :: Theme -> Alignment -> String
dzenCmd t a = unwords
    [ "dzen2"
    , "-e", "'onstart=lower'"
    , "-h", show $ decoHeight t
    , "-ta", case a of
        AlignLeft -> "l"
        AlignCenter -> "c"
        AlignRight -> "r"
    , "-fn", quote $ fontName t
    , "-bg", quote $ inactiveColor t
    , "-fg", quote $ inactiveTextColor t
    , "-w", "839"
    ]

trayer :: (Integral a) => Theme -> a -> Maybe a -> String
trayer t w m = unwords $
    [ "trayer"
    , "--SetPartialStrut true"
    , "--widthtype request"
    , "--transparent true"
    , "--alpha 0"
    , "--tint", "0x" ++ tail (inactiveColor t)
    , "--expand true"
    , "--height", show (decoHeight t)
    , "--width", show w
    , "--edge top"
    , "--align right"
    ] ++ maybe [] (\x -> ["--margin", show x]) m

quote :: String -> String
quote s = "'" ++ s ++ "'"

-- A dzen pretty printer. Icons will be used from directory 'p'.
-- Colours will be used from the theme 't'.
dzenPP' :: FilePath -> Theme -> PP
dzenPP' p t = dzenPP
    { ppCurrent         = dzenColor (activeTextColor t) (activeColor t) . pad
    , ppHidden          = ppInactive
    , ppHiddenNoWindows = if hiddenEnabled then ppInactive else const ""
    , ppUrgent          = dzenColor (urgentTextColor t) (urgentColor t) . dzenStrip
    , ppTitle           = dzenColor (activeTextColor t) (activeColor t) . padTo 80 . shorten 80
    , ppLayout          = ppInactive . \l -> case l of
        "Tall"         -> dzIcon "tall"
        "Mirror Tall"  -> dzIcon "mtall"
        "Full"         -> dzIcon "full"
    } where ppInactive = dzenColor (inactiveTextColor t) (inactiveColor t) . pad
            dzIcon = wrap "^i(" ".xbm)" . (p </>)

-- Pad a string with spaces to be at least n characters long.
padTo :: Int -> String -> String
padTo n s = wrap " " (replicate (n - length s - 1) ' ') s

dzen p t = statusBar (dzenCmd t AlignLeft) (dzenPP' p t) toggleStrutsKey

main = do
    dzenDir <- getAppUserDataDirectory "dzen"
    spawn (trayer myTheme trayerWidth (Just 65))
    spawn $ dzenDir </> "dzen_status | " ++ (dzenCmd myTheme AlignRight) ++ " -x 839"
    xmonad . withUrgencyHook NoUrgencyHook =<< dzen (dzenDir </> "icons") myTheme myConfig

