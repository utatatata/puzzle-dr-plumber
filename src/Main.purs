module Main where

import Prelude
import Control.Comonad (extract)
import Data.List (List(..))
import Data.List.Zipper (Zipper(..), DoubleZipper(..), down, left, right, up)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence)
import Data.Unfoldable (replicate)
import Effect (Effect)
import Effect.Console (log)
import Effect.Random (randomInt)

main :: Effect Unit
main = do
  log "🍝"

data Color
  = Red
  | Blue
  | Yellow

derive instance eqColor :: Eq Color

data Block
  = Capsule Color
  | Virus Color

derive instance eqBlock :: Eq Block

data Field
  = Field (DoubleZipper (Maybe Block))

isPlacable :: Field -> Block -> Boolean
isPlacable (Field dz) b =
  maybe false identity
    do
      u <- join $ extract <$> up dz
      uu <- join $ extract <$> (up =<< up dz)
      d <- join $ extract <$> down dz
      dd <- join $ extract <$> (down =<< down dz)
      l <- join $ extract <$> left dz
      ll <- join $ extract <$> (left =<< left dz)
      r <- join $ extract <$> right dz
      rr <- join $ extract <$> (right =<< right dz)
      pure let
        uuu = uu == u
        ub = u == b
        ddd = dd == d
        db = d == b
        lll = ll == l
        lb = l == b
        rrr = rr == r
        rb = r == b
      in
        uuu && ub || ub && db || db && ddd ||
        lll && lb || lb && rb || rb && rrr

empty :: Field
empty = Field (DoubleZipper (Zipper Nil Nothing (replicate (rows - 1) rowZ)))
  where
  rowZ = Zipper Nil Nothing (replicate (cols - 1) Nothing)
  rows = 16
  cols = 8

-- init :: Effect Field
-- init =
--   where
--   randomVirus = Virus <$> randomColor
--   randomColor = case randomInt 1 3 of
--     1 -> Red
--     2 -> Blue
--     _ -> Yellow