{-# LANGUAGE NoImplicitPrelude, TypeFamilies, TemplateHaskell, RankNTypes, FlexibleInstances, FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Graphics.UI.Bottle.Widgets.Layout
    ( Layout
    , Alignment
    , empty
    , AbsAlignedWidget, absAlignedWidget
    , alignment, widget, asTuple, width
    , fromCenteredWidget

    , addBefore, addAfter

    , Orientation(..)
    , box, hbox, vbox

    , scaleAround
    , scale
    , pad
    , hoverInPlaceOf
    ) where

import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Alignment (Alignment(..))
import qualified Graphics.UI.Bottle.Alignment as Alignment
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import           Graphics.UI.Bottle.Widgets.Box (Orientation(..))

import           Prelude.Compat

type AbsAlignedWidget a = (Vector2 Widget.R, Widget a)

axis :: Orientation -> Lens' Alignment Widget.R
axis Horizontal = _1
axis Vertical = _2

data BoxComponents a = BoxComponents
    { __widgetsBefore :: [a]
    , _focalWidget :: a
    , __widgetsAfter :: [a]
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''BoxComponents

data Layout a = Layout
    { _alignment :: Alignment
    , _widget :: Widget a
    }
Lens.makeLenses ''Layout

{-# INLINE width #-}
width :: Lens' (Layout a) Widget.R
width = widget . Widget.width

{-# INLINE asTuple #-}
asTuple ::
    Lens.Iso (Layout a) (Layout b) (Alignment, Widget a) (Alignment, Widget b)
asTuple =
    Lens.iso toTup fromTup
    where
        toTup w = (w ^. alignment, w ^. widget)
        fromTup (a, w) = Layout a w

{-# INLINE absAlignedWidget #-}
absAlignedWidget ::
    Lens.Iso (Layout a) (Layout b) (AbsAlignedWidget a) (AbsAlignedWidget b)
absAlignedWidget =
    asTuple . Lens.iso (f ((*) . (^. Alignment.ratio))) (f (fmap Alignment . (/)))
    where
        f op w = w & _1 %~ (`op` (w ^. _2 . Widget.size))

boxComponentsToWidget ::
    Orientation -> BoxComponents (Layout a) -> Layout a
boxComponentsToWidget orientation boxComponents =
    Layout
    { _alignment = boxAlign ^. focalWidget
    , _widget = boxWidget
    }
    where
        (boxAlign, boxWidget) =
            boxComponents <&> (^. asTuple)
            & Box.make orientation

fromCenteredWidget :: Widget a -> Layout a
fromCenteredWidget w = Layout 0.5 w

empty :: Layout a
empty = fromCenteredWidget Widget.empty

addBefore :: Orientation -> [Layout a] -> Layout a -> Layout a
addBefore orientation befores layout =
    BoxComponents befores layout []
    & boxComponentsToWidget orientation
addAfter :: Orientation -> [Layout a] -> Layout a -> Layout a
addAfter orientation afters layout =
    BoxComponents [] layout afters
    & boxComponentsToWidget orientation

-- The axisAlignment is the alignment point to choose within the resulting box
-- i.e: Horizontal box -> choose eventual horizontal alignment point
box :: Orientation -> Widget.R -> [Layout a] -> Layout a
box orientation axisAlignment layouts =
    componentsFromList layouts
    & boxComponentsToWidget orientation
    & alignment . axis orientation .~ axisAlignment
    where
        componentsFromList [] = BoxComponents [] (Layout 0 Widget.empty) []
        componentsFromList (w:ws) = BoxComponents [] w ws

hbox :: Widget.R -> [Layout a] -> Layout a
hbox = box Horizontal

vbox :: Widget.R -> [Layout a] -> Layout a
vbox = box Vertical

-- | scale = scaleAround 0.5
--   scaleFromTopMiddle = scaleAround (Vector2 0.5 0)
scaleAround :: Alignment -> Vector2 Widget.R -> Layout a -> Layout a
scaleAround (Alignment point) ratio (Layout (Alignment align) w) =
    Layout
    { _alignment = point + (align - point) / ratio & Alignment
    , _widget = Widget.scale ratio w
    }

scale :: Vector2 Widget.R -> Layout a -> Layout a
scale ratio = widget %~ Widget.scale ratio

pad :: Vector2 Widget.R -> Layout a -> Layout a
pad padding (Layout (Alignment align) w) =
    Layout
    { _alignment =
        (align * (w ^. Widget.size) + padding) / (paddedWidget ^. Widget.size)
        & Alignment
    , _widget = paddedWidget
    }
    where
        paddedWidget = Widget.pad padding w

-- Resize a layout to be the same alignment/size as another layout
hoverInPlaceOf :: Layout a -> Layout a -> Layout a
layout `hoverInPlaceOf` src =
    ( srcAbsAlignment
    , layoutWidget
        & Widget.translate (srcAbsAlignment - layoutAbsAlignment)
        & Widget.size .~ srcSize
    ) ^. Lens.from absAlignedWidget
    where
        (layoutAbsAlignment, layoutWidget) = layout ^. absAlignedWidget
        (srcAbsAlignment, srcWidget) = src ^. absAlignedWidget
        srcSize = srcWidget ^. Widget.size
