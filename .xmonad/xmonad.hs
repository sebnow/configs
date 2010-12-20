import qualified Data.Map as M
import XMonad
import XMonad.Hooks.DynamicLog hiding (dzen)
import XMonad.Layout.Decoration (Theme(..), defaultTheme)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Util.Cursor
import XMonad.Util.Themes (ThemeInfo(..))

myConfig = defaultConfig
    { terminal           = "urxvtc"
    , normalBorderColor  = inactiveBorderColor myTheme
    , focusedBorderColor = activeBorderColor myTheme
    , borderWidth        = 1
    , modMask            = mod4Mask
    , workspaces         = ["1:main", "2:web", "3:chat"] ++ map show [4 .. 9]
    , keys               = \c -> myKeys c `M.union` keys defaultConfig c
    , startupHook        = setDefaultCursor xC_left_ptr
    , layoutHook         = smartBorders $ layoutHook defaultConfig
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
        , urgentColor         = "#f57900"
        , urgentTextColor     = "#2e3436"
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

trayer :: (Integral a) => Theme -> a -> String
trayer t w = unwords
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
    ]

quote :: String -> String
quote s = "'" ++ s ++ "'"

dzenPP' :: Theme -> PP
dzenPP' t = dzenPP
    { ppCurrent         = dzenColor (activeTextColor t) (activeColor t) . pad
    , ppHidden          = ppInactive
    , ppHiddenNoWindows = ppInactive
    , ppUrgent          = dzenColor (urgentTextColor t) (urgentColor t) . dzenStrip
    , ppTitle           = dzenColor (activeTextColor t) (activeColor t) . pad . shorten 80
    , ppLayout          = ppInactive
    } where ppInactive = dzenColor (inactiveTextColor t) (inactiveColor t) . pad

dzen t = statusBar (dzenCmd t AlignLeft) (dzenPP' t) toggleStrutsKey

main = spawn (trayer myTheme trayerWidth) >> dzen myTheme myConfig >>= xmonad

