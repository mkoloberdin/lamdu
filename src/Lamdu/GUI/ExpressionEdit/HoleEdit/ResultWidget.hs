{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Lamdu.GUI.ExpressionEdit.HoleEdit.ResultWidget
    ( make
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Transaction (Transaction)
import qualified Data.Text as Text
import           GUI.Momentu (Widget, WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.Rect (Rect(..))
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import           Lamdu.Config (HasConfig(..))
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui (ExpressionN)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Parens as AddParens
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

getSearchStringRemainder ::
    (MonadReader env f, GuiState.HasState env) =>
    HoleWidgetIds.WidgetIds -> Sugar.Expression name m a -> f Text
getSearchStringRemainder widgetIds holeResultConverted
    | (`Lens.has` holeResultConverted) `any` [literalNum, wrappedExpr . literalNum] =
        HoleState.readSearchTerm widgetIds
        <&> \x -> if "." `Text.isSuffixOf` x then "." else ""
    | otherwise = pure mempty
    where
        literalNum = Sugar.rBody . Sugar._BodyLiteral . Sugar._LiteralNum
        wrappedExpr = Sugar.rBody . Sugar._BodyWrapper . Sugar.wExpr

setFocalAreaToFullSize :: WithTextPos (Widget a) -> WithTextPos (Widget a)
setFocalAreaToFullSize =
    Align.tValue . Widget.sizedState <. Widget._StateFocused . Lens.mapped . Widget.fFocalAreas .@~
    (:[]) . Rect 0

postProcessSugar :: Int -> ExpressionN m () -> ExpressionN m ExprGui.Payload
postProcessSugar minOpPrec expr =
    expr
    & AddParens.addWith minOpPrec
    <&> pl
    & SugarLens.wrappedExprs . Sugar.plData . ExprGui.plShowAnnotation
    .~ ExprGui.alwaysShowAnnotations
    where
        pl (x, needParens, ()) =
            ExprGui.Payload
            { ExprGui._plStoredEntityIds = []
            , ExprGui._plNearestHoles = NearestHoles.none
            , ExprGui._plShowAnnotation = ExprGui.neverShowAnnotations
            , ExprGui._plNeedParens = needParens == AddParens.NeedsParens
            , ExprGui._plMinOpPrec = x
            }

-- | Remove unwanted event handlers from a hole result
removeUnwanted :: (MonadReader env m, HasConfig env) => m (EventMap a -> EventMap a)
removeUnwanted =
    Lens.view config
    <&>
    \c ->
    concat
    [ Config.delKeys c
    , Config.enterSubexpressionKeys c
    , Config.leaveSubexpressionKeys c
    , Grid.stdKeys ^.. Lens.folded
    , Config.letAddItemKeys c
    ]
    <&> MetaKey.toModKey
    <&> E.KeyEvent MetaKey.KeyState'Pressed
    & E.deleteKeys

applyResultLayout :: Responsive a -> WithTextPos (Widget a)
applyResultLayout fGui =
    (fGui ^. Responsive.render)
    Responsive.LayoutParams
    { Responsive._layoutMode = Responsive.LayoutWide
    , Responsive._layoutContext = Responsive.LayoutClear
    }

make ::
    Monad m =>
    Sugar.Payload f ExprGui.Payload ->
    Widget.Id ->
    Sugar.HoleResult (T m) (Sugar.Expression (Name (T m)) (T m) ()) ->
    ExprGuiM m
    ( EventMap (T m GuiState.Update)
    , ExprGuiM m (WithTextPos (Widget (T m GuiState.Update)))
    )
make pl resultId holeResult =
    do
        c <- Lens.view Config.config
        let holeConfig = Config.hole c
        let pickAndMoveToNextHole =
                case pl ^. Sugar.plData . ExprGui.plNearestHoles . NearestHoles.next of
                Just nextHoleEntityId | Lens.has Lens._Nothing mFirstHoleInside ->
                    E.keysEventMapMovesCursor
                    (Config.holePickAndMoveToNextHoleKeys holeConfig)
                    (E.Doc ["Edit", "Result", "Pick, Move to next hole"])
                    (WidgetIds.fromEntityId nextHoleEntityId <$ pick)
                _ -> mempty
        let simplePickEventMap =
                E.keysEventMapMovesCursor
                (mappend Config.holePickResultKeys Config.holePickAndMoveToNextHoleKeys holeConfig)
                (E.Doc ["Edit", "Result", "Pick"]) pick
        let pickEventMap = pickAndMoveToNextHole <> simplePickEventMap
        searchStringRemainder <- getSearchStringRemainder widgetIds holeResultConverted
        let preEvent =
                Widget.PreEvent
                { Widget._pDesc = "Pick"
                , Widget._pAction = pick <&> GuiState.updateCursor
                , Widget._pTextRemainder = searchStringRemainder
                }
        remUnwanted <- removeUnwanted
        pure
            ( pickEventMap
            , postProcessSugar (pl ^. Sugar.plData . ExprGui.plMinOpPrec) holeResultConverted
                & ExprGuiM.makeSubexpression
                <&> Widget.enterResultCursor .~ resultId
                <&> Widget.widget . Widget.eventMapMaker . Lens.mapped %~ remUnwanted
                & GuiState.assignCursor resultId idWithinResultWidget
                <&> applyResultLayout
                <&> setFocalAreaToFullSize
                <&> Align.tValue %~ Widget.addPreEvent preEvent
                <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~ mappend pickEventMap
            )
    where
        pick = idWithinResultWidget <$ holeResult ^. Sugar.holeResultPick
        widgetIds = pl ^. Sugar.plEntityId & HoleWidgetIds.make
        holeResultId =
            holeResultConverted ^. Sugar.rPayload . Sugar.plEntityId
            & WidgetIds.fromEntityId
        mFirstHoleInside =
            holeResult ^?
            Sugar.holeResultConverted . SugarLens.holeAndWrapperPayloads . Sugar.plEntityId
            <&> WidgetIds.fromEntityId
        idWithinResultWidget = fromMaybe holeResultId mFirstHoleInside
        holeResultConverted = holeResult ^. Sugar.holeResultConverted
