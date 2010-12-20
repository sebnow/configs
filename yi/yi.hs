import Yi
import Yi.Prelude
import Prelude ()
import qualified Yi.Keymap.Vim (keymap)
import Yi.Style
import Yi.Style.Library
import Data.Monoid

-- Tango colour theme
tangoTheme :: Theme
tangoTheme = defaultTheme `override` \super _ -> super
  { baseAttributes = emptyAttributes
  , modelineAttributes = emptyAttributes
    { background = tangoGrey
    }
  , modelineFocusStyle = withFg Default
  , tabBarAttributes = emptyAttributes
  , blockCommentStyle = withFg tangoBlue
  , commentStyle = withFg tangoBlue
  , keywordStyle = withFg tangoBlack
  , longStringStyle = withFg tangoRed
  , numberStyle = withFg tangoGreen
  , operatorStyle = withFg tangoPurple
  , preprocessorStyle = withFg tangoGreen
  , stringStyle = withFg tangoRed
  , typeStyle = withFg tangoOrange
  , variableStyle = withFg tangoBlue
  }
  where tangoBlack = RGB 48 52 54
        tangoBlue = RGB 25 74 135
        tangoGreen = RGB 79 155 0
        tangoGrey = RGB 85 87 83
        tangoOrange = RGB 209 91 0
        tangoPurple = RGB 92 53 102
        tangoRed = RGB 167 0 0
        tangoWhite = RGB 255 255 255

main :: IO ()
main = yi $ defaultConfig
  { defaultKm = Yi.Keymap.Vim.keymap
  , configKillringAccumulate = False
  , startFrontEnd = startFrontEnd defaultConfig
  , configUI = (configUI defaultConfig)
    { configTheme = tangoTheme
    }
  }
