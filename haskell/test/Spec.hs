import Advent
import Test.Hspec

main :: IO ()
main = hspec do
  describe "AoC 2024" do
    it "day 1.1" do
      (day1_1 <$> readFile "test/day1.test") `shouldReturn` 11
    it "day 1.2" do
      (day1_2 <$> readFile "test/day1.test") `shouldReturn` 31
    it "day 2.1" do
      (day2_1 <$> readFile "test/day2.test") `shouldReturn` 2
    it "day 2.2" do
      (day2_2 <$> readFile "test/day2.test") `shouldReturn` 4
