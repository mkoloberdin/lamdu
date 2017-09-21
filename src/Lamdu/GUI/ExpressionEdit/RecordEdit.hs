{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.RecordEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Transaction (Transaction)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/), (/|/))
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.View as View
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.GUI.ExpressionEdit.Composite (destCursorId)
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

shouldAddBg :: Sugar.Composite name m a -> Bool
shouldAddBg (Sugar.Composite [] Sugar.ClosedComposite{} _) = False
shouldAddBg _ = True

make ::
    Monad m =>
    Sugar.Composite (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make record@(Sugar.Composite fields recordTail addField) pl =
    do
        config <- Lens.view Config.config
        (gui, resultPicker) <-
            ExprGuiM.listenResultPicker $
            do
                fieldsGui <- makeFieldsWidget fields myId
                case recordTail of
                    Sugar.ClosedComposite actions ->
                        E.weakerEvents (closedRecordEventMap config actions) fieldsGui
                        & return
                    Sugar.OpenComposite rest ->
                        makeOpenRecord fieldsGui rest (Widget.toAnimId myId)
        let addFieldEventMap =
                addField
                <&> (^. Sugar.cairNewTag . Sugar.tagInstance)
                <&> WidgetIds.fromEntityId
                <&> TagEdit.diveToRecordTag
                & Widget.keysEventMapMovesCursor (Config.recordAddFieldKeys config)
                  (E.Doc ["Edit", "Record", "Add Field"])
                & ExprGuiM.withHolePicker resultPicker
        (if addBg then ExpressionGui.addValFrame else return id)
            ?? E.weakerEvents addFieldEventMap gui
    & ExpressionGui.stdWrapParentExpr pl (destCursorId fields (pl ^. Sugar.plEntityId))
    where
        myId = WidgetIds.fromExprPayload pl
        addBg = shouldAddBg record

makeFieldRow ::
    Monad m =>
    Sugar.CompositeItem (Name m) m (Sugar.Expression (Name m) m ExprGuiT.Payload) ->
    ExprGuiM m
    ( WithTextPos (Widget (T m Widget.EventResult))
    , ExpressionGui m
    )
makeFieldRow (Sugar.CompositeItem delete tag fieldExpr) =
    do
        config <- Lens.view Config.config
        let itemEventMap = recordDelEventMap config delete
        tagLabel <-
            TagEdit.makeRecordTag TagEdit.WithTagHoles (ExprGuiT.nextHolesBefore fieldExpr) tag
            <&> Align.tValue %~ E.weakerEvents itemEventMap
        hspace <- Spacer.stdHSpace
        fieldGui <- ExprGuiM.makeSubexpression fieldExpr
        return (tagLabel /|/ hspace, E.weakerEvents itemEventMap fieldGui)

makeFieldsWidget ::
    Monad m =>
    [Sugar.CompositeItem (Name m) m (Sugar.Expression (Name m) m ExprGuiT.Payload)] ->
    Widget.Id -> ExprGuiM m (ExpressionGui m)
makeFieldsWidget [] myId =
    (Widget.makeFocusableView ?? myId <&> (Align.tValue %~))
    <*> ExpressionGui.grammarLabel "()"
    <&> Responsive.fromWithTextPos
makeFieldsWidget fields _ =
    Responsive.taggedList <*> mapM makeFieldRow fields

separationBar :: Theme.CodeForegroundColors -> Widget.R -> Anim.AnimId -> View
separationBar theme width animId =
    View.unitSquare (animId <> ["tailsep"])
    & Element.tint (Theme.recordTailColor theme)
    & Element.scale (Vector2 width 10)

makeOpenRecord ::
    Monad m =>
    ExpressionGui m -> ExprGuiT.SugarExpr m -> AnimId ->
    ExprGuiM m (ExpressionGui m)
makeOpenRecord fieldsGui rest animId =
    do
        theme <- Lens.view Theme.theme
        vspace <- Spacer.stdVSpace
        restExpr <-
            ExpressionGui.addValPadding
            <*> ExprGuiM.makeSubexpression rest
        return $ fieldsGui & Responsive.render . Lens.imapped %@~
            \layoutMode fields ->
            let restW = (restExpr ^. Responsive.render) layoutMode
                minWidth = restW ^. Element.width
                targetWidth = fields ^. Element.width
            in
            fields
            /-/
            separationBar (Theme.codeForegroundColors theme) (max minWidth targetWidth) animId
            /-/
            vspace
            /-/
            restW

closedRecordEventMap ::
    Monad m =>
    Config -> Sugar.ClosedCompositeActions m ->
    Widget.EventMap (T m Widget.EventResult)
closedRecordEventMap config (Sugar.ClosedCompositeActions open) =
    Widget.keysEventMapMovesCursor (Config.recordOpenKeys config)
    (E.Doc ["Edit", "Record", "Open"]) $ WidgetIds.fromEntityId <$> open

recordDelEventMap ::
    Monad m =>
    Config -> m Sugar.EntityId -> Widget.EventMap (m Widget.EventResult)
recordDelEventMap config delete =
    Widget.keysEventMapMovesCursor (Config.delKeys config)
    (E.Doc ["Edit", "Record", "Delete Field"]) $ WidgetIds.fromEntityId <$> delete
