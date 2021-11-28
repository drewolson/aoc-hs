module Aoc.PreludeSpec
  ( spec,
  )
where

import Aoc.Prelude qualified as Prelude
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "tshow" do
    it "converts a showable to text" do
      let result = Prelude.tshow [True]

      result `shouldBe` ("[True]" :: Text)
