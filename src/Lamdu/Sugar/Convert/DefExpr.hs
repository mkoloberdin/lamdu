{-# LANGUAGE ScopedTypeVariables #-}
module Lamdu.Sugar.Convert.DefExpr
    ( convert
    ) where

import           AST (Tree, Pure, ann)
import           AST.Infer (irType)
import           AST.Term.Scheme (saveScheme)
import           AST.Unify.Generalize (generalize)
import qualified Control.Lens as Lens
import qualified Data.Property as Property
import           Lamdu.Calc.Infer (alphaEq, runPureInfer)
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Expr.IRef (DefI)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Convert.Binder (convertDefinitionBinder)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.Type as ConvertType
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)
import           Text.PrettyPrint.HughesPJClass (Pretty(..))

import           Lamdu.Prelude

type T = Transaction

convert ::
    (Monoid a, Monad m) =>
    Tree Pure T.Scheme -> Definition.Expr (Val (Input.Payload m a)) -> DefI m ->
    ConvertM m (DefinitionBody InternalName (T m) (T m) (ConvertPayload m a))
convert defType defExpr defI =
    do
        (presMode, content) <-
            convertDefinitionBinder defI (defExpr ^. Definition.expr)
        inferContext <- Lens.view ConvertM.scInferContext
        let inferredType =
                generalize (defExpr ^. Definition.expr . ann . Input.inferResult . irType)
                >>= saveScheme
                & runPureInfer V.emptyScope inferContext
                & (^?! Lens._Right . Lens._1)
        unless (alphaEq defType inferredType) $
            fail $ "Def type mismatches its inferred type! " <> show (pPrint (defType, inferredType))
        defTypeS <- ConvertType.convertScheme (EntityId.currentTypeOf entityId) defType
        DefinitionBodyExpression DefinitionExpression
            { _deType = defTypeS
            , _dePresentationMode = presMode <&> (^. Property.mkProperty)
            , _deContent = content
            } & pure
    where
        entityId = ExprIRef.globalId defI & EntityId.ofBinder
