import Data.List (intercalate)
import XMonad
import XMonad.Layout.Decoration (Theme(..), defaultTheme)
import XMonad.Util.Themes (ThemeInfo(..))
import qualified Data.Map as M

myConfig = defaultConfig
    { terminal           = "urxvtc"
    , normalBorderColor  = inactiveBorderColor myTheme
    , focusedBorderColor = activeBorderColor myTheme
    , borderWidth        = 1
    , modMask            = mod4Mask
    , workspaces         = ["1:main", "2:web", "3:chat"] ++ map show [4 .. 9]
    , keys               = \c -> myKeys c `M.union` keys defaultConfig c
    }

-- Key bindings {{{
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList
    [ ((modMask, xK_p), spawn $ themedDmenu myTheme) -- program launcher
    ]
-- }}}

-- Themes {{{
tangoTheme :: ThemeInfo
tangoTheme = TI
    { themeName        = "Tango!"
    , themeAuthor      = "Sebastian Nowicki"
    , themeDescription = "Tango Project color scheme"
    , theme = defaultTheme
        { activeColor         = "#2e3436"
        , activeTextColor     = "#eeeeec"
        , activeBorderColor   = "#555753"
        , inactiveColor       = "#555753"
        , inactiveTextColor   = "#d3d7cf"
        , inactiveBorderColor = "#2e3436"
        , urgentColor         = "#f57900"
        , urgentTextColor     = "#fcaf3e"
        , urgentBorderColor   = "#ce5c00"
        }
    }

myTheme :: Theme
myTheme = (theme tangoTheme)
    { fontName = "-*-terminus-medium-*-*-*-*-*-*-*-*-*-*-*"
    }
-- }}}

-- Return dmenu cmd specifying an XMonad theme
themedDmenu t = intercalate " " cmd
    where cmd = [ "dmenu_run"
                , "-fn", q $ fontName t
                , "-nb", q $ inactiveColor t
                , "-nf", q $ inactiveTextColor t
                , "-sb", q $ activeColor t
                , "-sf", q $ activeTextColor t
                ]
          q s = "\"" ++ s ++ "\"" -- Quote string

main = xmonad myConfig

