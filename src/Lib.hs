module Lib where

import OldLib
import Control.Monad ( forM_ )

data DataValue =  Num Int
                | Quote String
                | Cons (DataValue, DataValue) 
                | Car (DataValue, DataValue)
                | Cdr (DataValue, DataValue)
                | Eq (DataValue, DataValue)
                | Atom DataValue
                | Add [DataValue]
                | Sub [DataValue]
                | Mul [DataValue]
                | Div (DataValue, DataValue)
                | Mod (DataValue, DataValue)
                | Less (DataValue, DataValue)
                | Def (String, DataValue)
                | Lambda ([String], String)--, Int
                deriving (Show, Eq)


parseSpace :: Parser String
parseSpace = parseMany (parseChar ' ')
            
parseTab :: Parser [String]
parseTab so = case parseAnd parseSpace (parseAllCharExcept " ") s of
    Just ((_, s'), "" ) -> Just([eraseAll " " s'] , "")
    Just ((_ , s') , r) -> case parseTab (tail r) of
        Just ( s'', r' ) -> Just(eraseAll " " s':s'' , r')
        Nothing -> Just([eraseAll " " s'] , "")
    Nothing -> Nothing    
    where s = reverse (cleanSpace (reverse so))
    
cleanSpace :: String -> String
cleanSpace "" = ""
cleanSpace (x:xs) = case x of
    ' ' -> cleanSpace xs
    _ -> x:xs 

parseInt :: Parser DataValue
parseInt "" =  Nothing 
parseInt s = case reads s ::[(Int, String)] of 
            [(x, s)]-> Just(Num x, s)
            _ -> Nothing
            

tabToCons::[String] -> DataValue
tabToCons [] = Quote "()"
tabToCons (x:xs) = case parseInt x of
    Just (i, s) -> Cons (i , tabToCons xs)
    Nothing -> Cons (Quote x , tabToCons xs)


--ng
parseQuotelong:: Parser DataValue
parseQuotelong "" = Nothing
parseQuotelong so  = case parseBracket s of
        Just (so', r) -> case parseAllCharExcept " " s' of
            Just ("quote", r') -> Just (Quote (cleanSpace r') , r)
            _ -> Nothing
            where s' = cleanSpace so'
        _ -> Nothing
        where s = cleanSpace so
         
parseQuote:: Parser DataValue
parseQuote "" = Nothing
parseQuote so  = case parseAnd (parseChar '\'') (parseAllCharExcept ")") s of
    Just ((a , "("), r) -> Just (Quote "()", tail r)
    Just ((a, x:xs) , r) ->  case x of
        '(' -> case parseTab xs of
            Just (tab, s') -> Just (tabToCons tab, s') 
            _ -> Nothing
        _ -> case parseAllCharExcept " )" ((x:xs) ++ r) of
            Just (a, b) -> Just (Quote a , b)
            _ -> Nothing
    _ -> Nothing
    where s = cleanSpace so

parseOp:: [DataValue] -> Parser DataValue
parseOp env "" = Nothing
parseOp env so = case parseBracket s of
            Just (so', r) -> case parseAllCharExcept " " s' of
                Just ("+", r') -> case parseMany (parseData env) r' of
                    Just (a, s'') -> Just (Add a, r)
                    _ -> Nothing
                Just ("-", r') -> case parseMany (parseData env) r' of
                    Just (a, s'') -> Just (Sub a, r)
                    _ -> Nothing
                Just ("*", r') -> case parseMany (parseData env) r' of
                    Just (a, s'') -> Just (Mul a, r)
                    _ -> Nothing
                Just ("div", r') -> case parseAnd (parseData env) (parseData env) r' of
                    Just ((a, b), s'') -> Just (Div (a, b), r)
                    _ -> Nothing
                Just ("mod", r') -> case parseAnd (parseData env) (parseData env) r' of
                    Just ((a, b), s'') -> Just (Mod (a, b), r)
                    _ -> Nothing
                Just ("<", r') -> case parseAnd (parseData env) (parseData env) r' of
                    Just ((a, b), s'') -> Just (Less (a, b), r)
                    _ -> Nothing
                _-> Nothing
                where s' = cleanSpace so'
            _ -> Nothing
            where s = cleanSpace so


getInsideBracket':: String ->  String  -> Int -> (String, String)
getInsideBracket' [] a i = (a, "")
getInsideBracket' (x:xs) a i = case x of
    '(' -> getInsideBracket' xs (a ++ [x]) (i+1)
    ')' -> case i of
        0 -> (a,  xs)
        i -> getInsideBracket' xs (a ++ [x]) (i-1)
    a' -> getInsideBracket' xs (a ++ [x])  i
    
getInsideBracket:: String -> (String, String)
getInsideBracket s  = getInsideBracket' s [] 0

parseBracket:: Parser String
parseBracket "" =  Nothing
parseBracket  so =  case (head s, tail s) of
    ('(', s') -> case getInsideBracket s' of
        (a, r) -> Just (a, r)
    _ -> Nothing
    where s = cleanSpace so
          
parseCons :: [DataValue] -> Parser DataValue
parseCons env "" = Nothing  
parseCons env so =  case parseBracket s of
    Just (so' , rest) -> case parseAllCharExcept " " s' of
        Just ("cons", r) -> case  parseAnd (parseData env) (parseData env) r of 
            Just ((a , b), s') -> Just (Cons (a, b), rest ) 
            _ -> Nothing
        Just ("eq?", r) -> case  parseAnd (parseData env) (parseData env) r of 
            Just ((a , b), s') -> Just (Eq (a, b), rest) 
            _ -> Nothing
        Just ("atom?", r) -> case parseData env r of 
            Just (a, s') -> Just (Atom a , rest) 
            _ -> Nothing
        _ -> Nothing
        where s' = cleanSpace so'
    _ -> Nothing
    where s = cleanSpace so

 
parseCarCdr :: [DataValue] ->  Parser DataValue
parseCarCdr env "" = Nothing  
parseCarCdr env so =  case (head s, tail s) of
    ('(', so') -> case parseAllCharExcept " " s' of
        Just ("car", r) -> case parseCons env r of
            Just (Cons (a, b), s'') -> Just (Car (a, b), s'') 
            _ -> Nothing 
        Just ("cdr", r) -> case parseCons env r of
            Just (Cons (a, b), s'') -> Just (Cdr (a, b), s'') 
            _ -> Nothing 
        _ -> Nothing
        where s' = cleanSpace so'
    _ -> Nothing
    where s = cleanSpace so


--addToEnv:: DataValue
--addToEnv d do env = env

getValueFromEnv:: [DataValue] -> String -> Maybe DataValue
getValueFromEnv [] key = Nothing
getValueFromEnv (x:env) key = case x of
    Def (k, value) -> (if k == key then Just value else getValueFromEnv env key)
    _ -> getValueFromEnv env key

parseEnvbracket::[DataValue] -> Parser DataValue
parseEnvbracket env "" = Nothing
parseEnvbracket env s = case parseBracket s  of
    Just (s', r) -> case parseAllCharExcept " " (cleanSpace s') of
        Just (key, r') -> case  getValueFromEnv  env key of
            Just val -> Just (val , r' ++ r)
            Nothing -> Nothing
        _  -> Nothing
    _  -> Nothing

parseEnv::[DataValue] -> Parser DataValue
parseEnv env "" = Nothing
parseEnv env s = case parseAllCharExcept " " (cleanSpace s)  of
    Just (s', r) ->case  getValueFromEnv  env s' of
        Just val -> Just (val , r)
        Nothing -> Nothing
    _  -> Nothing

parseDefLam::[DataValue] -> Parser DataValue
parseDefLam env "" = Nothing
parseDefLam env so = case parseBracket s  of
    Just (s', r) -> case parseAllCharExcept " " (cleanSpace s') of
        Just ("define", r') -> case parseAllCharExcept " " (cleanSpace r') of
            Just (key, r'') -> case parseData env r'' of
                Just (val , r''') ->  Just (Def (key, val), r)  
                _ -> Nothing
            _ -> Nothing
        Just ("lambda", r') -> case parseBracket (cleanSpace r') of
            Just (argsO, r'') -> case parseTab argsO of
                Just (args , r''') ->  Just (Lambda (args, cleanSpace r''), cleanSpace r) 
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing
    Nothing -> Nothing
    where s = cleanSpace so

parseData:: [DataValue] -> Parser DataValue
parseData env "" = Nothing
parseData env so = case parseInt s  of
    Just(i , s') -> Just (i , s')
    _ -> case  parseQuote s of
        Just (q , s') -> Just (q , s')
        _ -> case parseCons env s of
            Just (cons, s') -> Just (cons, s')
            _ -> case parseCarCdr env s of
                Just (car, s') -> Just (car, s')
                _ -> case parseOp env s of
                    Just (op , s') -> Just (op , s')
                    _ -> case parseQuotelong s of
                        Just (q , s') -> Just (q , s')
                        _ -> case parseDefLam env s of
                            Just(def , s') -> Just( def , s') 
                            _ -> case parseEnv env s of
                                Just( env , s') -> Just( env , s') 
                                _ -> case parseEnvbracket env s of
                                    Just( env , s') -> Just( env , s') 
                                    _ -> Nothing
    where s = cleanSpace so            


showBool:: Bool -> String
showBool True = "#t"
showBool False = "#f"

getCons'::DataValue -> Int -> String
getCons' d 0 = '(':getCons' d 1
getCons' d i =  case d of
    Cons (a, Cons (b, c)) -> getValue a ++ " " ++ getCons'  (Cons (b, c)) 1
    Cons (a , Quote "()") -> getValue a ++ ")" 
    Cons (a, b) ->  getValue a ++ " . " ++ getValue b ++ ")" 
    _ -> ""

getCons::DataValue ->  String
getCons s = getCons' s 0

evalOp:: DataValue -> Int
evalOp d = case d of
    Num a -> a 
    Sub a -> evalSub a
    Add a -> evalAdd a
    Mul a -> evalMul a
    Div a -> evalDiv a
    Mod a -> evalMod a
    _ -> 0

evalLess:: (DataValue, DataValue) -> Bool
evalLess (a , b) = evalOp a < evalOp b
    

evalMod:: (DataValue, DataValue) -> Int
evalMod (a , b) = evalOp a `mod` evalOp b 

evalDiv:: (DataValue, DataValue) -> Int
evalDiv (a , b) = case  evalOp b of 
    0 -> 0 --Error
    b' -> evalOp a `div` b'


evalMul':: [DataValue] -> Int -> Int
evalMul' [] i = i
evalMul' (x:xs) i = case x of
    Num a -> evalMul' xs (i * a)
    Mul a -> evalMul' xs (i * evalMul a)
    Sub a -> evalMul' xs (i * evalSub a)
    Add a -> evalMul' xs (i * evalAdd a)
    _ -> 0 -- Error

evalMul:: [DataValue] -> Int
evalMul d = evalMul' d 1

evalSub':: [DataValue] -> Int -> Int
evalSub' [] i = i
evalSub' (x:xs) i = case x of
    Num a -> evalSub' xs (i - a )
    Sub a -> evalSub' xs (i - evalSub a)
    Add a -> evalSub' xs (i - evalAdd a)
    Mul a -> evalSub' xs (i - evalMul a)
    _ -> 0 -- Error

evalSub:: [DataValue] -> Int
evalSub [] = 0 --error
evalSub (x:xs) = evalSub' xs (evalOp x)

evalAdd':: [DataValue] -> Int -> Int
evalAdd' [] i = i
evalAdd' (x:xs) i = case x of
    Num a -> evalAdd' xs (i + a)
    Add a -> evalAdd' xs (i + evalAdd a)
    Sub a -> evalAdd' xs (i + evalSub a)
    Mul a -> evalAdd' xs (i + evalMul a)
    _ -> 0 -- Error

evalAdd:: [DataValue] -> Int
evalAdd d = evalAdd' d 0

getValue::DataValue-> String
getValue d = case  d of
    Quote "()" -> ""
    Quote s -> s
    Num i -> show i
    Cons a -> getCons (Cons a)
    Car (a, b) -> getValue a
    Cdr (a, b) -> getValue b
    Eq (a , b) -> showBool (a == b)
    Atom (Cons a) -> "#f"
    Atom a -> "#t"
    Add a -> show (evalAdd  a)
    Sub a -> show (evalSub  a)
    Mul a -> show (evalMul  a)
    Div a -> show (evalDiv  a)
    Mod a -> show (evalMod a)
    Less a ->  showBool (evalLess a)
    Def (s, d) -> getValue d
    Lambda (args, func ) -> "#<procedure>"

addBraket:: String -> String
addBraket s | ')' `elem` s = '(' : s
            | '.' `elem` s = ['('] ++ s ++ [')']
            | otherwise = s


isHead':: String -> String -> String -> Bool
isHead' str [] ret = True
isHead' [] key  ret = False
isHead' (x:xs) (x':xs') ret = (x == x') && isHead' xs xs' ret

isHead:: String -> String ->  Bool
isHead  key str= isHead' str key key

remouveXelem:: Int ->[a] -> [a]
remouveXelem i a | i <= 0 = a
remouveXelem _ [] = [] 
remouveXelem i (x:xs) = remouveXelem (i - 1) xs  

replace :: String -> String -> String -> String
replace ori new [] = []
replace ori new s = if isHead ori s then
    new ++ replace ori new (remouveXelem (length ori) s)
    else head s : replace ori new (tail s)

evalLambda:: [DataValue] -> [String] ->  [DataValue] -> String -> [DataValue]
evalLambda env [] r finalexp = parseLisp env finalexp ++ r
evalLambda env (arg:r) (d:r') func = evalLambda env r r' (replace arg (getValue d) func)
evalLambda env (arg:r) _ _ = []

evalLisp::[DataValue] ->  [DataValue] ->  [DataValue]
evalLisp _ [] = []
evalLisp env (x:xs)  = case x of
    Lambda (args , func) -> if  length args <= length xs 
        then evalLambda env args xs func
        else Lambda (args , func) : evalLisp env xs
    a -> a :  evalLisp  env xs

parseLisp::[DataValue] ->   String ->  [DataValue]
parseLisp env  s = case parseData env s of
    Just (d , s') -> case d of
        Def a -> case parseLisp (env ++ [Def a]) s' of
            a' -> [Def a] ++ a'
        a ->  case parseLisp env  s' of
            a' -> a:a'
    Nothing ->  []

printDataValues:: DataValue -> IO()
printDataValues d = case d of
    Def a ->  putStr ""
    a -> putStrLn (getValue a)

evalAndPrintLisp:: String -> IO()
evalAndPrintLisp s = case parseLisp []  s  of
    [] -> print "Error"
    a -> mapM_ printDataValues (evalLisp a a)
    --_ -> [ print "Error"]