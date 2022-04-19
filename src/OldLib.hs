module OldLib where

type Parser a = String -> Maybe (a ,String)


parseChar :: Char -> Parser Char
parseChar a  "" = Nothing
parseChar a  str | a == head str = Just (a, tail str)
                 |  otherwise = Nothing

parseAnd::Parser a -> Parser b -> Parser(a, b)
parseAnd  p1 p2 s = case p1 s of
    Just( r1 , "") -> Nothing
    Just( r1 , s') -> case p2 s' of
        Just (r2 , s'') -> Just ((r1, r2), s'')
        Nothing -> Nothing
    Nothing -> Nothing

purre :: a -> Parser a
purre x s =  Just (x, s)

parseMany:: Parser a -> Parser [a]
parseMany p "" =  purre [] ""
parseMany p s = case p s of 
    Just (r, s') -> case  parseMany p s' of
        Just (r' , s'') -> Just (r:r', s'')
        Nothing -> Just([r], s')
    Nothing -> purre [] s
            

parseCharExcept:: String -> Parser Char
parseCharExcept e (h:r)
                | h `elem` e = Nothing
                | otherwise = Just (h , r)
parseCharExcept _ _ = Nothing

parseAllCharExcept:: String -> Parser String
parseAllCharExcept e  = parseMany (parseCharExcept e) 


eraseAll:: String -> String -> String
eraseAll c = eraseAll' c []

eraseAll':: String -> String -> String -> String
eraseAll' c res [] = reverse res
eraseAll' c res (a:r)
        | a `elem` c = eraseAll' c res r
        | otherwise = eraseAll' c (a:res) r 
