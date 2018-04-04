{-# LANGUAGE FlexibleContexts #-}

module Lamdu.GUI.EvalView
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Monad (zipWithM)
import qualified Control.Monad.Reader as Reader
import qualified Data.List as List
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..), WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue ((/-/), (/|/), hbox, vbox)
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.View (View(..))
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.GridView as GridView
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Graphics.DrawingCombinators ((%%))
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Formatting (Format(..))
import           Lamdu.GUI.ExpressionEdit.TagEdit (makeTagView)
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import           Lamdu.Sugar.Types (ResVal)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

textView ::
    ( MonadReader env m, Element.HasAnimIdPrefix env, TextView.HasStyle env
    ) => Text -> m (WithTextPos View)
textView text = (TextView.make ?? text) <*> Lens.view Element.animIdPrefix

makeField ::
    (Monad i, Monad o) =>
    Sugar.TagInfo (Name f) -> ResVal (Name g) -> ExprGuiM i o [Aligned View]
makeField tag val =
    do
        tagView <- makeTagView tag
        space <- Spacer.stdHSpace
        valView <- makeInner val
        pure
            [ toAligned 1 tagView
            , Aligned 0.5 space
            , toAligned 0 valView
            ]
    where
        toAligned x (WithTextPos y w) =
            Aligned (Vector2 x (y / w ^. Element.height)) w

makeError ::
    ( MonadReader env m, Element.HasAnimIdPrefix env, TextView.HasStyle env
    ) => Sugar.EvalError -> m (WithTextPos View)
makeError err =
    textView msg & Reader.local (Element.animIdPrefix <>~ ["error"])
    where
        msg =
            case err of
            Sugar.EvalHole -> "?"
            _ -> Text.pack (show err)

arrayCutoff :: Int
arrayCutoff = 10

tableCutoff :: Int
tableCutoff = 6

makeTable ::
    (Monad i, Monad o) =>
    Sugar.ResTable (Name f) (ResVal (Name g)) -> ExprGuiM i o (WithTextPos View)
makeTable (Sugar.ResTable headers valss) =
    do
        header <- mapM makeTagView headers
        rows <- take (tableCutoff-1) valss & traverse . traverse %%~ makeInner
        s <- Spacer.stdHSpace
        let table =
                header : rows <&> traverse %~ (^. Align.tValue)
                <&> List.intersperse s
                <&> traverse %~ Aligned 0.5
                & GridView.make & snd & Align.WithTextPos 0
        remainView <-
            if null (drop tableCutoff rows)
            then pure Element.empty
            else TextView.makeLabel "…"
        Aligned 0.5 table /-/ Aligned 0.5 remainView ^. Align.value & pure

makeArray ::
    (Monad i, Monad o) =>
    [ResVal (Name f)] -> ExprGuiM i o (WithTextPos View)
makeArray items =
    do
        itemViews <- zipWith makeItem [0..arrayCutoff] items & sequence
        opener <- TextView.makeLabel "["
        closer <- TextView.makeLabel "]"
        opener : itemViews ++ [closer] & hbox & pure
    where
        makeItem idx val =
            [ [ TextView.makeLabel ", " | idx > 0 ]
            , [ makeInner val
                | idx < arrayCutoff ]
            , [ TextView.makeLabel "…" | idx == arrayCutoff ]
            ] & concat
            & sequence
            <&> hbox
            & Reader.local (Element.animIdPrefix %~ Anim.augmentId (idx :: Int))

makeTree ::
    (Monad i, Monad o) =>
    Sugar.ResTree (ResVal (Name f)) -> ExprGuiM i o (WithTextPos View)
makeTree (Sugar.ResTree root subtrees) =
    do
        rootView <- makeInner root
        subtreeViews <- zipWithM makeItem [0..cutoff] subtrees
        rootView : subtreeViews & vbox & pure
    where
        makeItem idx val =
            [ [ TextView.makeLabel "* " ]
            , [ makeInner val | idx < cutoff ]
            , [ TextView.makeLabel "…" | idx == cutoff ]
            ] & concat
            & sequence
            <&> hbox
            & Reader.local (Element.animIdPrefix %~ Anim.augmentId (idx :: Int))
        cutoff = 4


makeRecord ::
    (Monad i, Monad o) =>
    Sugar.ResRecord (Name f) (ResVal (Name g)) -> ExprGuiM i o (WithTextPos View)
makeRecord (Sugar.ResRecord fields) =
    traverse (uncurry makeField) fields <&> GridView.make <&> snd
    <&> Align.WithTextPos 0

makeStream ::
    (Monad i, Monad o) =>
    Sugar.ResStream (ResVal (Name f)) -> ExprGuiM i o (WithTextPos View)
makeStream (Sugar.ResStream head_) =
    do
        o <- TextView.makeLabel "["
        inner <- makeInner head_
        c <- TextView.makeLabel ", …]" <&> (^. Align.tValue)
        o /|/ inner
            & Align.tValue %~ (hGlueAlign 1 ?? c)
            & pure
    where
        hGlueAlign align l r = (Aligned align l /|/ Aligned align r) ^. Align.value

makeInject ::
    (Monad i, Monad o) =>
    Sugar.ResInject (Name f) (ResVal (Name g)) -> ExprGuiM i o (WithTextPos View)
makeInject (Sugar.ResInject tag mVal) =
    do
        tagView <- makeTagView tag
        case mVal of
            Nothing -> pure tagView
            Just val ->
                do
                    s <- Spacer.stdHSpace
                    i <- makeInner val
                    tagView /|/ s /|/ i & pure

depthCounts :: ResVal name -> [Int]
depthCounts v =
    v ^.. Sugar.resBody . Lens.folded
    & take arrayCutoff
    <&> depthCounts
    & List.transpose
    <&> sum
    & (1 :)

-- Make animation frames of eval views animate from the whole rect
fixSize :: WithTextPos View -> WithTextPos View
fixSize view =
    view & Align.tValue . View.animFrames . Anim.frameImages . traverse %~ onImage
    where
        size = view ^. Element.size
        onImage image =
            image
            & Anim.iRect . Rect.size .~ size
            & Anim.iUnitImage %~
            (DrawUtils.scale (image ^. Anim.iRect . Rect.size / view ^. Element.size) %%)

makeInner ::
    (Monad i, Monad o) => ResVal (Name f) -> ExprGuiM i o (WithTextPos View)
makeInner (Sugar.ResVal entityId body) =
    case body of
    Sugar.RError err -> makeError err
    Sugar.RFunc{} -> textView "Fn"
    Sugar.RInject inject -> makeInject inject
    Sugar.RRecord record -> makeRecord record
    Sugar.RText txt -> toText txt
    Sugar.RBytes x -> toText x
    Sugar.RFloat x -> toText x
    Sugar.RArray x -> makeArray x
    Sugar.RTree x -> makeTree x
    Sugar.RTable x -> makeTable x
    Sugar.RStream x -> makeStream x
    & advanceDepth
    & Reader.local (Element.animIdPrefix .~ animId)
    where
        animId = WidgetIds.fromEntityId entityId & Widget.toAnimId
        -- Only cut non-leaf expressions due to depth limits
        advanceDepth
            | Lens.has Lens.folded body = ExprGuiM.advanceDepth pure
            | otherwise = id

toText ::
    ( Format r, MonadReader env m, TextView.HasStyle env
    , Element.HasAnimIdPrefix env
    ) => r -> m (WithTextPos View)
toText val =
    textView cut
    where
        text = format val
        cut =
            map limLine start ++
            ( case rest of
              [] -> []
              _ -> ["…"]
            ) & Text.intercalate "\n"
            where
                (start, rest) = splitAt 10 (Text.lines text)
        limLine :: Text -> Text
        limLine ln =
            start <>
            if Text.null rest
            then ""
            else "…"
            where
                (start, rest) = Text.splitAt 100 ln

make ::
    (Monad i, Monad o) =>
    ResVal (Name f) -> ExprGuiM i o (WithTextPos View)
make v =
    do
        maxEvalViewSize <- Lens.view (Theme.theme . Theme.maxEvalViewSize)
        let depthLimit =
                depthCounts v & scanl (+) 0 & tail
                & takeWhile (< maxEvalViewSize) & length
        makeInner v & ExprGuiM.resetDepth depthLimit
    <&> fixSize
