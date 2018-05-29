{-# LANGUAGE TemplateHaskell #-}

module Lamdu.Sugar.Types.Hole
    ( ValNames(..), valNamesGlobalDefs, valNamesParams, valNamesNominals, valNamesTags
    , HoleOption(..), hoVal, hoValNames, hoSugaredBaseExpr, hoResults
    , HoleOption'
    , Literal(..), _LiteralNum, _LiteralBytes, _LiteralText
    , OptionLiteral
    , Hole(..), holeOptions, holeOptionLiteral, holeMDelete
    , HoleResultScore(..), hrsNumFragments, hrsScore
    , HoleResult(..)
        , holeResultConverted
        , holeResultPick
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.ListT (ListT)
import           Data.Functor.Identity (Identity(..))
import           Lamdu.Calc.Val.Annotated (Val)
import           Lamdu.Sugar.Internal.EntityId (EntityId)

import           Lamdu.Prelude

data HoleResultScore = HoleResultScore
    { _hrsNumFragments :: !Int
    , _hrsScore :: ![Int]
    } deriving (Eq, Ord, Generic)

data HoleResult o resultExpr = HoleResult
    { _holeResultConverted :: resultExpr
    , _holeResultPick :: o ()
    } deriving (Functor, Foldable, Traversable, Generic)

data ValNames name = ValNames
    { _valNamesGlobalDefs :: [name]
    , _valNamesParams :: [name]
    , _valNamesNominals :: [name]
    , _valNamesTags :: [name]
    } deriving (Functor, Foldable, Traversable, Generic)

instance Semigroup (ValNames name) where
    ValNames x0 x1 x2 x3 <> ValNames y0 y1 y2 y3 =
        ValNames (x0 <> y0) (x1 <> y1) (x2 <> y2) (x3 <> y3)

instance Monoid (ValNames name) where
    mempty = ValNames [] [] [] []

data HoleOption name i o resultExpr = HoleOption
    { _hoVal :: Val ()
    , _hoValNames :: i (ValNames name)
    , _hoSugaredBaseExpr :: i resultExpr
    , -- A group in the hole results based on this option
      _hoResults :: ListT i (HoleResultScore, i (HoleResult o resultExpr))
    } deriving (Functor, Generic)

type HoleOption' name m = HoleOption name m m

data Literal f
    = LiteralNum (f Double)
    | LiteralBytes (f ByteString)
    | LiteralText (f Text)
    deriving Generic

type OptionLiteral i o resultExpr =
    Literal Identity -> i (HoleResultScore, i (HoleResult o resultExpr))

data Hole name i o resultExpr = Hole
    { _holeOptions :: i [HoleOption name i o resultExpr]
      -- TODO: Lifter from i to o?
    , _holeOptionLiteral :: OptionLiteral i o resultExpr
    , -- Changes the structure around the hole to remove the hole.
      -- For example (f _) becomes (f) or (2 + _) becomes 2
      _holeMDelete :: Maybe (o EntityId)
    } deriving (Functor, Generic)

Lens.makeLenses ''Hole
Lens.makeLenses ''HoleOption
Lens.makeLenses ''HoleResult
Lens.makeLenses ''HoleResultScore
Lens.makeLenses ''ValNames
Lens.makePrisms ''Literal
