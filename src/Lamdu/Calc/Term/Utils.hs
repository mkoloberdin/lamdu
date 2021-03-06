{-# LANGUAGE TemplateHaskell #-}

module Lamdu.Calc.Term.Utils
    ( Composite(..), tags, rest
    , case_
    , recExtend
    , culledSubexprPayloads
    ) where

import           AST (Tree, Ann(..), monoChildren)
import           AST.Term.Row (RowExtend(..))
import qualified Control.Lens as Lens
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T

import           Lamdu.Prelude

-- | Return all subexprs until the given cut-point
culledSubexprPayloads :: (a -> Bool) -> Val a -> [a]
culledSubexprPayloads cut (Ann pl body)
    | cut pl = []
    | otherwise = pl : body ^. monoChildren . Lens.to (culledSubexprPayloads cut)

data Composite a = Composite
    { _tags :: Map T.Tag a
    , _rest :: Maybe a
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''Composite

case_ :: Tree (RowExtend T.Tag V.Term V.Term) (Ann pl) -> Composite (Val pl)
case_ (RowExtend tag handler r) =
    caseVal r
    & tags . Lens.at tag ?~ handler
    where
        caseVal v@(Ann _ body) =
            case body of
            V.BLeaf V.LAbsurd -> Composite mempty Nothing
            V.BCase x -> case_ x
            _ -> Composite mempty (Just v)

recExtend :: Tree (RowExtend T.Tag V.Term V.Term) (Ann pl) -> Composite (Val pl)
recExtend (RowExtend tag field r) =
    recExtendVal r
    & tags . Lens.at tag ?~ field
    where
        recExtendVal v@(Ann _ body) =
            case body of
            V.BLeaf V.LRecEmpty -> Composite mempty Nothing
            V.BRecExtend x -> recExtend x
            _ -> Composite mempty (Just v)
