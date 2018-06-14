{-# LANGUAGE NamedFieldPuns, DisambiguateRecordFields #-}
-- | Load the sugared code

module Lamdu.GUI.CodeEdit.Load
    ( loadWorkArea
    ) where

import           Data.CurAndPrev (CurAndPrev(..))
import           Data.Functor.Identity (Identity(..))
import           Data.Orphans () -- Imported for Monoid (IO ()) instance
import           Data.Property (MkProperty')
import qualified Lamdu.Cache as Cache
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Debug as Debug
import           Lamdu.Eval.Results (EvalResults)
import           Lamdu.Expr.IRef (ValI)
import qualified Lamdu.GUI.AnnotationsPass as AnnotationsPass
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Convert as SugarConvert
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Names.Add as AddNames
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Parens as AddParens
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

toExprGuiMPayload ::
    ( AddParens.MinOpPrec, AddParens.NeedsParens
    , ( ExprGui.ShowAnnotation
      , ([Sugar.EntityId], NearestHoles)
      )
    ) -> ExprGui.Payload
toExprGuiMPayload (minOpPrec, needParens, (showAnn, (entityIds, nearestHoles))) =
    ExprGui.Payload entityIds nearestHoles showAnn
    (needParens == AddParens.NeedsParens)
    minOpPrec

traverseAddNearestHoles ::
    Traversable t =>
    t (Sugar.Expression name i o a) ->
    t (Sugar.Expression name i o (a, NearestHoles))
traverseAddNearestHoles = NearestHoles.add traverse

exprAddNearestHoles ::
    Sugar.Expression name i o a ->
    Sugar.Expression name i o (a, NearestHoles)
exprAddNearestHoles expr =
    Identity expr
    & traverseAddNearestHoles
    & runIdentity

postProcessExpr ::
    Sugar.Expression (Name n) i o ([Sugar.EntityId], NearestHoles) ->
    Sugar.Expression (Name n) i o ExprGui.Payload
postProcessExpr =
    fmap toExprGuiMPayload . AddParens.add . AnnotationsPass.markAnnotationsToDisplay

getNameProp :: Monad m => Anchors.CodeAnchors m -> T.Tag -> MkProperty' (T m) Text
getNameProp = DataOps.assocPublishedTagName . Anchors.tags

loadWorkArea ::
    (HasCallStack, Monad m) =>
    Cache.Functions -> Debug.Monitors -> CurAndPrev (EvalResults (ValI m)) ->
    Anchors.CodeAnchors m ->
    T m (Sugar.WorkArea (Name (T m)) (T m) (T m) ExprGui.Payload)
loadWorkArea cache monitors theEvalResults cp =
    SugarConvert.loadWorkArea cache monitors theEvalResults cp
    >>= report . AddNames.addToWorkArea (getNameProp cp)
    <&>
    \Sugar.WorkArea { _waPanes, _waRepl, _waGlobals } ->
    Sugar.WorkArea
    { _waPanes = _waPanes <&> Sugar.paneDefinition %~ traverseAddNearestHoles
    , _waRepl = _waRepl & Sugar.replExpr %~ exprAddNearestHoles
    , _waGlobals = _waGlobals
    }
    & SugarLens.workAreaExpressions %~ postProcessExpr
    where
        Debug.EvaluatorM report = monitors ^. Debug.naming . Debug.mAction
