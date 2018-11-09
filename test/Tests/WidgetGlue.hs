{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
module Tests.WidgetGlue
    ( test
    ) where

import qualified Control.Lens as Lens
import           Data.Semigroup (First(..), Last(..))
import           GUI.Momentu
import           GUI.Momentu.Glue
-- import qualified GUI.Momentu.Hover as Hover
import           GUI.Momentu.Widget (R, Widget(..))
import qualified GUI.Momentu.Widget as Widget
import           Generic.Random
import           Test.Lamdu.Prelude
import           Test.QuickCheck

data Direction = Forward | Backward
    deriving (Eq, Ord, Generic, Show)

data IsStrollable = IsStrollable | NotStrollable
    deriving (Eq, Ord, Generic, Show)

instance Arbitrary Direction where arbitrary = genericArbitrary uniform
instance Arbitrary IsStrollable where arbitrary = genericArbitrary uniform

data GlueTrees tree = GlueTrees Direction Orientation tree UnfocusedWidgetTree
    deriving (Show, Generic)

data FocusedWidgetTree
    = FocusedLeaf (Vector2 R)
    | FocusedHover Direction (GlueTrees FocusedWidgetTree)
    | FocusedGlue (GlueTrees FocusedWidgetTree)
    deriving (Show, Generic)

data UnfocusedWidgetTree
    = UnfocusedLeaf Widget.Id IsStrollable (Vector2 R)
    | UnfocusedGlue (GlueTrees UnfocusedWidgetTree)
    deriving (Show, Generic)

data SomeWidgetTree
    = WidgetFocused FocusedWidgetTree
    | WidgetUnfocused UnfocusedWidgetTree
    deriving (Show, Generic)

arbitraryUnfocusedLeaf :: Gen UnfocusedWidgetTree
arbitraryUnfocusedLeaf = UnfocusedLeaf <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary tree => Arbitrary (GlueTrees tree) where
    arbitrary = genericArbitraryRec uniform

instance Arbitrary FocusedWidgetTree where
    arbitrary = genericArbitraryRec uniform `withBaseCase` (FocusedLeaf <$> arbitrary)

instance Arbitrary UnfocusedWidgetTree where
    arbitrary = genericArbitraryRec uniform `withBaseCase` arbitraryUnfocusedLeaf

instance Arbitrary SomeWidgetTree where
    arbitrary = genericArbitraryRec uniform

mkFocused :: Widget.Surrounding -> Widget.Focused (f Update)
mkFocused _ =
    Widget.Focused
    { Widget._fFocalAreas = []
    , Widget._fEventMap = mempty
    , Widget._fPreEvents = mempty
    , Widget._fMEnterPoint = Nothing -- TODO: Enter?
    , Widget._fLayers = mempty
    }

dired :: GluesTo a b c => Direction -> Orientation -> (a -> b -> c)
dired Forward o = glue o
dired Backward o = flip (glue o)

glueToWidget ::
    Applicative f => (a -> Gui Widget f) -> GlueTrees a -> Widget (f Update)
glueToWidget f (GlueTrees dir o w0 w1) =
    dired dir o (f w0) (toWidgetUnfocused w1)

toWidgetUnfocused :: Applicative f => UnfocusedWidgetTree -> Gui Widget f
toWidgetUnfocused (UnfocusedLeaf myId isStrollable size) =
    Widget.StateUnfocused Widget.Unfocused
    { Widget._uMEnter = Nothing -- TODO
    , Widget._uMStroll =
        case isStrollable of
        NotStrollable -> Nothing
        IsStrollable -> Just (First myId, Last myId)
    , Widget._uLayers = mempty
    } & Widget size
toWidgetUnfocused (UnfocusedGlue g) = glueToWidget toWidgetUnfocused g

toWidgetFocused :: Applicative f => FocusedWidgetTree -> Gui Widget f
toWidgetFocused (FocusedLeaf size) = Widget size (Widget.StateFocused mkFocused)
toWidgetFocused (FocusedGlue g) = glueToWidget toWidgetFocused g
toWidgetFocused (FocusedHover which g) = _toHoverWidget which g

-- env :: a
-- env = undefined

-- hover = Hover.hover env

-- toHoverWidget which (GlueTrees dir o w0 w1) =
--     case dir of
--     Forward -> glue o
--     dired dir o (Hover.hover hoverer)
--     ho
--     case dir) of
--     (Forward, Forward) -> glue o (hover widget0) widget1
--     (Backward, Forward) -> glue o widget0 (hover widget1)
--     _ -> error "TODO"
--     & Hover.hoverInPlaceOf _
--     where
--         (primary, hoverer) =
--             case which of
--             Forward -> (widget0, widget1)
--             Backward -> (widget1, widget0)
--         widget0 = toWidgetFocused w0
--         widget1 = toWidgetUnfocused w1

toWidget :: Applicative f => SomeWidgetTree -> Gui Widget f
toWidget (WidgetFocused f) = toWidgetFocused f
toWidget (WidgetUnfocused f) = toWidgetUnfocused f

toWidgetI :: SomeWidgetTree -> Gui Widget Lens.Identity
toWidgetI = toWidget

propWidgetGlue :: SomeWidgetTree -> Bool
propWidgetGlue tree = Widget.isFocused (toWidgetI tree)

test :: Test
test = testProperty "various widget glue" propWidgetGlue
