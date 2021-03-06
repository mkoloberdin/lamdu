{-# LANGUAGE TemplateHaskell, DerivingVia #-}
module Lamdu.Name
    ( Stored, CollisionSuffix
    , Collision(..), _NoCollision, _Collision
    , visible
    , TagText(..), ttText, ttCollision
    , StoredName(..), snProp, snDisplayText, snTagCollision
    , Name(..), _AutoGenerated, _Stored
    , isValidText
    ) where

import qualified Control.Lens as Lens
import qualified Data.Char as Char
import           Data.Property (Property)
import qualified Data.Text as Text
import qualified Lamdu.CharClassification as Chars
import qualified Lamdu.I18N.Name as Texts
import           Lamdu.Precedence (HasPrecedence(..))

import           Lamdu.Prelude

type Stored = Text

type CollisionSuffix = Int

data Collision
    = NoCollision
    | Collision CollisionSuffix
    | UnknownCollision -- we have a collision but unknown suffix (inside hole result)
    deriving (Show, Generic, Eq)

data TagText = TagText
    { _ttText :: Text
    , _ttCollision :: Collision
    } deriving (Show, Generic, Eq)

data StoredName o = StoredName
    { _snProp :: Property o Text
    , _snDisplayText :: TagText
    , _snTagCollision :: Collision
    } deriving Generic

data Name o
    = AutoGenerated Text
    | Stored (StoredName o)
    | Unnamed CollisionSuffix
    deriving Generic

visible ::
    (MonadReader env m, Has (Texts.Name Text) env) =>
    Name o -> m (TagText, Collision)
visible (Stored (StoredName _ disp tagCollision)) = pure (disp, tagCollision)
visible (AutoGenerated name) = pure (TagText name NoCollision, NoCollision)
visible (Unnamed suffix) =
    Lens.view (has . Texts.unnamed) <&>
    \x -> (TagText x NoCollision, Collision suffix)

Lens.makeLenses ''StoredName
Lens.makeLenses ''TagText
Lens.makePrisms ''Collision
Lens.makePrisms ''Name

instance Show (Name o) where
    show (AutoGenerated text) = unwords ["(AutoName", show text, ")"]
    show (Unnamed suffix) = unwords ["(Unnamed", show suffix, ")"]
    show (Stored (StoredName _ disp collision)) =
        unwords ["(StoredName", show disp, show collision, ")"]

instance HasPrecedence (Name o) where
    precedence (Stored (StoredName _ disp _)) =
        disp ^? ttText . Lens.ix 0 . Lens.to precedence & fromMaybe 12
    precedence _ = 12

isValidText :: Text -> Bool
isValidText x =
    Text.all Char.isAlphaNum x
    || Text.all (`elem` Chars.operator) x
