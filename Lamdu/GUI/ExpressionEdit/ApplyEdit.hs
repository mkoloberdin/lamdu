{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.ApplyEdit
    ( make, prefixPrecedence
    ) where

import qualified Control.Lens as Lens
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Lamdu.CharClassification as CharClassification
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.Precedence as Prec
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Names.Get as NamesGet
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

prefixPrecedence :: Int
prefixPrecedence = 10

mkPrecedence :: Monad m => Sugar.Apply name (ExprGuiT.SugarExpr m) -> Int
mkPrecedence (Sugar.Apply func specialArgs _annotatedArgs) =
    case specialArgs of
    Sugar.NoSpecialArgs -> 0
    Sugar.ObjectArg{} -> prefixPrecedence
    Sugar.InfixArgs _ _ ->
        case NamesGet.fromExpression func <&> Text.unpack . nName of
        [x:_] -> CharClassification.charPrecedence x
        _ -> 20

infixMarker :: Vector2 Anim.R -> Draw.Image ()
infixMarker (Vector2 w h) =
    mconcat
    [ Draw.line (x, 0) (0,x)
    , Draw.line (w-x, 0) (w,x)
    , Draw.line (w-x, h) (w,h-x)
    , Draw.line (x, h) (0,h-x)
    , Draw.line (0, x) (0, h-x)
    , Draw.line (w, x) (w, h-x)
    , Draw.line (x, 0) (w-x, 0)
    , Draw.line (x, h) (w-x, h)
    ]
    <&> const ()
    where
        x = min w h / 4

addInfixMarker :: Widget.Id -> Widget a -> Widget a
addInfixMarker widgetId widget =
    widget
    & Widget.bottomFrame
    <>~ Anim.simpleFrame frameId (infixMarker (widget ^. Widget.size))
    where
        frameId = Widget.toAnimId widgetId ++ ["infix"]

makeInfixFuncName ::
    Monad m => ExprGuiT.SugarExpr m -> ExprGuiM m (ExpressionGui m)
makeInfixFuncName func =
    do
        res <- ExprGuiM.makeSubexpressionWith 0 (const prec) func
        if any BinderEdit.nonOperatorName (NamesGet.fromExpression func)
            then
                res
                & TreeLayout.widget %~
                    addInfixMarker (WidgetIds.fromExprPayload (func ^. Sugar.rPayload))
                & return
            else return res
    where
        -- TODO: What precedence to give when it must be atomic?:
        prec = Prec.make 20

isBoxed :: Sugar.Apply name a -> Bool
isBoxed apply = Lens.has (Sugar.aAnnotatedArgs . Lens.traversed) apply

makeFuncRow ::
    Monad m =>
    Maybe AnimId ->
    Int ->
    Sugar.Apply name (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeFuncRow mParensId prec apply pl =
    case specialArgs of
    Sugar.NoSpecialArgs ->
        ExprGuiM.makeSubexpressionWith 0 (const (Prec.make prec)) func
        & overrideFuncEventMap
    Sugar.ObjectArg arg ->
        ExpressionGui.combineSpaced mParensId
        <*> sequenceA
        [ ExprGuiM.makeSubexpressionWith 0
          (ExpressionGui.after .~ prec+1) func
          & overrideFuncEventMap
        , ExprGuiM.makeSubexpressionWith
          (if isBoxed apply then 0 else prec)
          (ExpressionGui.before .~ prec) arg
        ]
    Sugar.InfixArgs l r ->
        ExpressionGui.combineSpaced mParensId
        <*> sequenceA
        [ ExpressionGui.combineSpaced Nothing
            <*> sequenceA
            [ ExprGuiM.makeSubexpressionWith 0 (ExpressionGui.after .~ prec) l
            , makeInfixFuncName func & overrideFuncEventMap
            ]
        , ExprGuiM.makeSubexpressionWith (prec+1) (ExpressionGui.before .~ prec+1) r
        ]
    where
        overrideFuncEventMap mkFunc
            | isBoxed apply =
                  do
                      (funcGui, holePicker) <- ExprGuiM.listenResultPicker mkFunc
                      funcEventMap <- ExprEventMap.modifyEventMap pl holePicker
                      funcGui & TreeLayout.widget %~ Widget.strongerEvents funcEventMap
                          & return
            | otherwise = mkFunc

        Sugar.Apply func specialArgs _ = apply

make ::
    Monad m =>
    Sugar.Apply (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make apply@(Sugar.Apply func _specialArgs annotatedArgs) pl =
    ExpressionGui.stdWrapParentExpr pl $ \myId ->
    do
        parentPrec <- ExprGuiM.outerPrecedence <&> Prec.ParentPrecedence
        let needParens =
                not (isBoxed apply)
                && Prec.needParens parentPrec (Prec.my prec)
        let mParensId
                | needParens = Just (Widget.toAnimId myId)
                | otherwise = Nothing
        makeFuncRow mParensId prec apply pl
            & ( if needParens
                then ExprGuiM.withLocalPrecedence 0 (const (Prec.make 0))
                else
                if (isBoxed apply)
                then mkBoxed annotatedArgs myId
                else id
              )
            & ExprGuiM.assignCursor myId funcId
    where
        funcId = func ^. Sugar.rPayload & WidgetIds.fromExprPayload
        prec = mkPrecedence apply

makeArgRows ::
    Monad m =>
    Sugar.AnnotatedArg (Name m) (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m)
makeArgRows arg =
    ExpressionGui.tagItem
    <*> TagEdit.makeParamTag (arg ^. Sugar.aaTag)
    <*> ExprGuiM.makeSubexpression (arg ^. Sugar.aaExpr)

mkBoxed ::
    Monad m =>
    [Sugar.AnnotatedArg (Name m) (ExprGuiT.SugarExpr m)] ->
    Widget.Id ->
    ExprGuiM m (ExpressionGui m) ->
    ExprGuiM m (ExpressionGui m)
mkBoxed annotatedArgs myId mkFuncRow =
    do
        argRows <- traverse makeArgRows annotatedArgs
        funcRow <- ExprGuiM.withLocalPrecedence 0 (const (Prec.make 0)) mkFuncRow
        vbox <- ExpressionGui.vboxTopFocalSpaced
        ExpressionGui.addValFrame myId
            ?? vbox
                ([funcRow, vbox argRows] <&> TreeLayout.alignment . _1 .~ 0)
