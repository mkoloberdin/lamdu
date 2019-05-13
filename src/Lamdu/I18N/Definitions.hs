-- | "Definition"-related texts
{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, DerivingVia, RankNTypes #-}
module Lamdu.I18N.Definitions where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH

import           Lamdu.Prelude

data Definitions a = Definitions
    { _newDefinitionButton :: a
    , _newDefinition :: a
    , _undelete :: a
    , _undeleteButton :: a
    , _defUpdateHeader :: a
    , _defUpdateTo :: a
    , _defUpdateWas :: a
    , _def :: a
    , _extractReplToDef :: a
    , _execRepl :: a
    , _extract :: a
    }
    deriving stock (Generic, Generic1, Eq, Functor, Foldable, Traversable)
    deriving Applicative via (Generically1 Definitions)
Lens.makeLenses ''Definitions
JsonTH.derivePrefixed "_" ''Definitions