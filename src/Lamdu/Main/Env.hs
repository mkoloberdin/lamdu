-- | The Environment threaded in Lamdu main
{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeApplications, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Lamdu.Main.Env
    ( Env(..)
    , evalRes
    , exportActions
    , config
    , theme
    , settings
    , style
    , mainLoop
    , animIdPrefix
    , debugMonitors
    , cachedFunctions
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property)
import qualified Data.Property as Property
import           GUI.Momentu.Animation.Id (AnimId)
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Main as MainLoop
import           GUI.Momentu.State (GUIState)
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Cache as Cache
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme(..))
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Data.Db.Layout (ViewM)
import qualified Lamdu.Debug as Debug
import qualified Lamdu.GUI.Main as GUIMain
import qualified Lamdu.GUI.VersionControl.Config as VCConfig
import           Lamdu.I18N.LangId (LangId)
import           Lamdu.I18N.Language (Language)
import qualified Lamdu.I18N.Language as Language
import           Lamdu.Settings (Settings(..))
import           Lamdu.Style (Style)
import qualified Lamdu.Style as Style

import           Lamdu.Prelude

data Env = Env
    { _evalRes :: GUIMain.EvalResults ViewM
    , _exportActions :: GUIMain.ExportActions ViewM
    , _config :: Config
    , _theme :: Theme
    , _settings :: Property IO Settings
    , _style :: Style.Style
    , _mainLoop :: MainLoop.Env
    , _animIdPrefix :: AnimId
    , _debugMonitors :: Debug.Monitors
    , _cachedFunctions :: Cache.Functions
    , _language :: Language
    }
Lens.makeLenses ''Env

instance Has (GUIMain.ExportActions ViewM) Env where has = exportActions
instance Has (GUIMain.EvalResults ViewM) Env where has = evalRes
instance Has Settings Env where has = settings . Property.pVal
instance Has Style Env where has = style
instance Has MainLoop.Env Env where has = mainLoop
instance Spacer.HasStdSpacing Env where stdSpacing = has . Theme.stdSpacing
instance GuiState.HasCursor Env
instance Has GUIState Env where has = mainLoop . has
instance Has TextEdit.Style Env where has = style . Style.base
instance Has TextView.Style Env where has = has @TextEdit.Style . has
instance Has Theme Env where has = theme
instance Has Config Env where has = config
instance Has Hover.Style Env where has = theme . has
instance Has VCConfig.Theme Env where has = has . Theme.versionControl
instance Has VCConfig.Config Env where has = has . Config.versionControl
instance Has Menu.Config Env where
    has = Menu.configLens (config . Config.menu) (theme . Theme.menu)
instance Has SearchMenu.TermStyle Env where has = theme . Theme.searchTerm
instance Has Debug.Monitors Env where has = debugMonitors
instance Has Cache.Functions Env where has = cachedFunctions
instance Element.HasAnimIdPrefix Env where animIdPrefix = animIdPrefix
instance Has Dir.Layout Env where has = language . has
instance Language.HasLanguage Env where language = language
instance Has LangId Env where has = language . has
instance Has (t Text) Language => Has (t Text) Env where has = language . has
