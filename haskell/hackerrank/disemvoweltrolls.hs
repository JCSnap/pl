{-
	-   it "should work for single words" $ do
		disemvowel "hat" `shouldBe` "ht"
		disemvowel "toast" `shouldBe` "tst"
		it "should work with spaces" $ do
		disemvowel "toast hat" `shouldBe` "tst ht"
		-}
import Data.Char (toLower)

disemvowel :: String -> String
disemvowel str = filter (\x -> toLower x `notElem` "aeiou") str
