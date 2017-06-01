{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Graphics.UI.Bottle.Widgets
    ( makeFocusableTextView, makeFocusableLabel
    , makeTextEdit
    , makeTextEditor, makeLineEdit, makeWordEdit
    , makeFocusDelegator
    , makeChoiceWidget
    , stdFontHeight
    , stdSpacing
    , stdHSpaceWidth, stdHSpaceView
    , stdVSpaceHeight, stdVSpaceView
    , vspacer
    , hboxCenteredSpaced
    ) where

import           Data.List (intersperse)
import           Data.Store.Property (Property)
import qualified Data.Store.Property as Property
import           Data.Text.Encoding (encodeUtf8)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as EventMap
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.View (View)
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Choice as Choice
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import           Graphics.UI.Bottle.WidgetsEnvT (WidgetEnvT)
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Graphics.UI.GLFW as GLFW

import           Lamdu.Prelude

makeFocusableTextView ::
    (Monad m, Applicative f) =>
    Text -> Widget.Id -> WidgetEnvT m (Widget (f Widget.EventResult))
makeFocusableTextView text myId =
    (Widget.makeFocusableView ?? myId)
    <*> (TextView.make ?? text ?? Widget.toAnimId myId <&> Widget.fromView)

makeFocusableLabel ::
    (Monad m, Applicative f) =>
    Text -> Widget.Id -> WidgetEnvT m (Widget (f Widget.EventResult))
makeFocusableLabel text myIdPrefix =
    makeFocusableTextView text (Widget.joinId myIdPrefix [encodeUtf8 text])

makeFocusDelegator ::
    (Monad m, Applicative f) =>
    FocusDelegator.Config ->
    FocusDelegator.FocusEntryTarget ->
    Widget.Id ->
    WidgetEnvT m (Widget (f Widget.EventResult) -> Widget (f Widget.EventResult))
makeFocusDelegator fdConfig focusEntryTarget myId =
    FocusDelegator.make fdConfig focusEntryTarget myId

makeTextEdit ::
    Monad m =>
    Text -> Widget.Id ->
    WidgetEnvT m (Widget (Text, Widget.EventResult))
makeTextEdit text myId =
    do
        style <- WE.readTextStyle
        TextEdit.make style text myId

makeTextEditor ::
    (Monad m, Applicative f) =>
    Property f Text -> Widget.Id ->
    WidgetEnvT m (Widget (f Widget.EventResult))
makeTextEditor textRef myId =
    makeTextEdit (Property.value textRef) myId
    <&> Widget.events %~ setter
    where
        setter (newText, eventRes) =
            eventRes <$
            when (newText /= Property.value textRef) (Property.set textRef newText)

deleteKeyEventHandler :: ModKey -> Widget a -> Widget a
deleteKeyEventHandler key =
    Widget.eventMap %~
    EventMap.deleteKey (EventMap.KeyEvent GLFW.KeyState'Pressed key)

-- TODO: Editor, not Edit (consistent with makeTextEditor vs. makeTextEdit)
makeLineEdit ::
    (Monad m, Applicative f) =>
    Property f Text ->
    Widget.Id ->
    WidgetEnvT m (Widget (f Widget.EventResult))
makeLineEdit textRef myId =
    makeTextEditor textRef myId <&> deleteKeyEventHandler (ModKey mempty GLFW.Key'Enter)

makeWordEdit ::
    (Monad m, Applicative f) =>
    Property f Text ->
    Widget.Id ->
    WidgetEnvT m (Widget (f Widget.EventResult))
makeWordEdit textRef myId =
    makeLineEdit textRef myId <&> deleteKeyEventHandler (ModKey mempty GLFW.Key'Space)

stdFont :: Monad m => WidgetEnvT m Draw.Font
stdFont = WE.readEnv <&> (^. WE.envTextStyle . TextEdit.sTextViewStyle . TextView.styleFont)

stdFontHeight :: Monad m => WidgetEnvT m Double
stdFontHeight = stdFont <&> Draw.fontHeight

stdVSpaceHeight :: Monad m => WidgetEnvT m Double
stdVSpaceHeight =
    (*)
    <$> stdFontHeight
    <*> (WE.readEnv <&> WE.stdSpacing <&> (^. _2))

stdHSpaceWidth :: Monad m => WidgetEnvT m Double
stdHSpaceWidth =
    (*)
    <$> (stdFont <&> (`Draw.textAdvance` " "))
    <*> (WE.readEnv <&> WE.stdSpacing <&> (^. _1))

stdSpacing :: Monad m => WidgetEnvT m (Vector2 Double)
stdSpacing = Vector2 <$> stdHSpaceWidth <*> stdVSpaceHeight

stdHSpaceView :: Monad m => WidgetEnvT m View
stdHSpaceView = stdHSpaceWidth <&> (`Vector2` 0) <&> Spacer.make

stdVSpaceView :: Monad m => WidgetEnvT m View
stdVSpaceView = stdVSpaceHeight <&> Vector2 0 <&> Spacer.make

-- | Vertical spacer as ratio of line height
vspacer :: Monad m => Double -> WidgetEnvT m (Widget f)
vspacer ratio = stdFontHeight <&> (ratio *) <&> Spacer.makeVertical <&> Widget.fromView

hboxCenteredSpaced :: Monad m => [Widget f] -> WidgetEnvT m (Widget f)
hboxCenteredSpaced widgets =
    stdHSpaceView
    <&> Widget.fromView
    <&> Box.hboxAlign 0.5 . (`intersperse` widgets)

makeChoiceWidget ::
    (Eq a, Monad m, Applicative f) =>
    (a -> f ()) -> [(a, Widget (f Widget.EventResult))] -> a ->
    Choice.Config -> Widget.Id -> WidgetEnvT m (Widget (f Widget.EventResult))
makeChoiceWidget choose children curChild choiceConfig myId =
    Choice.make choiceConfig (children <&> annotate) myId
    where
        annotate (item, widget) =
            ( if item == curChild then Choice.Selected else Choice.NotSelected
            , choose item
            , widget
            )
