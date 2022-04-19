import Test.Tasty.Hspec
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Lib

specsBasicBlock :: Spec
specsBasicBlock = do
    --Cons 
    it "parseInt Test 0" $ 
     parseTab "   25" `shouldBe` Just (Num 25 ,"")
    it "parseCons Test 0" $ 
     parseCons  "(cons 1 2)" `shouldBe`
     Just (Cons (Num 1,Num 2),"")
    it "parseCons Test 1" $ 
      parseCons  "(foo, 12 ,   ro    , po , 12     )" `shouldBe`
      Just (Cons [Token "foo", Number 12, Token "ro", Token "po", Number 12],"")
    --
    

main :: IO ()
main = do
        hspec specsBasicBlock
        putStrLn " "
