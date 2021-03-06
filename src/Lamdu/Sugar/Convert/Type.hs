-- | Convert Lamdu.Calc.Type datatypes to sugared counterparts

module Lamdu.Sugar.Convert.Type
    ( convertType
    , convertScheme
    ) where

import           AST (Tree, Pure(..))
import           AST.Term.FuncType (FuncType(..))
import           AST.Term.Nominal (NominalInst(..))
import           AST.Term.Row (RowExtend(..))
import qualified AST.Term.Scheme as S
import           Control.Monad.Transaction (MonadTransaction)
import qualified Data.Map as Map
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Anchors (anonTag)
import qualified Lamdu.Sugar.Convert.TId as ConvertTId
import qualified Lamdu.Sugar.Convert.Tag as ConvertTag
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Internal.EntityId
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convertComposite ::
    MonadTransaction n m =>
    EntityId -> Tree Pure T.Row ->
    m (CompositeFields InternalName (Type InternalName))
convertComposite entityId (MkPure (T.RExtend (RowExtend tag typ rest))) =
    do
        typS <- convertType (EntityId.ofTypeOf entityId) typ
        convertComposite (EntityId.ofRestOfComposite entityId) rest
            <&> compositeFields %~ ((tagS, typS): )
    where
        tagS = ConvertTag.withoutContext entityId tag

convertComposite _ (MkPure (T.RVar v)) =
    CompositeFields mempty (Just (nameWithContext v anonTag)) & pure
convertComposite _ (MkPure T.REmpty) = CompositeFields mempty Nothing & pure

convertType :: MonadTransaction n m => EntityId -> Tree Pure T.Type -> m (Type InternalName)
convertType entityId (MkPure typ) =
    case typ of
    T.TVar tv -> nameWithContext tv anonTag & TVar & pure
    T.TFun (FuncType param res) ->
        TFun
        <$> convertType (ofFunParam entityId) param
        <*> convertType (ofFunResult entityId) res
    T.TInst (NominalInst tid args)
        | Map.null rParams ->
            TInst
            <$> ConvertTId.convert tid
            <*> (Map.toList tParams & traverse convertTypeParam)
        | otherwise -> error "Currently row-params are unsupported"
        where
            T.Types (S.QVarInstances tParams) (S.QVarInstances rParams) = args
            convertTypeParam (tv, val) =
                (,)
                <$> taggedName tv
                <*> convertType (EntityId.ofTInstParam tv entityId) val
    T.TRecord composite -> TRecord <$> convertComposite entityId composite
    T.TVariant composite -> TVariant <$> convertComposite entityId composite
    <&> Type entityId

convertScheme :: MonadTransaction n m => EntityId -> Tree Pure T.Scheme -> m (Scheme InternalName)
convertScheme entityId (MkPure (S.Scheme tvs typ)) =
    Scheme tvs <$> convertType entityId typ
