import System.IO
import System.Random

type Ident = String

data NExpr a = 
	Var Ident | Const a | 
	Plus (NExpr a) (NExpr a) | Minus (NExpr a) (NExpr a) | Times (NExpr a) (NExpr a)
	deriving(Read)

data BExpr a = 
	OR (BExpr a) (BExpr a) | AND (BExpr a) (BExpr a) | NOT (BExpr a) |
	Gt (NExpr a) (NExpr a) | Eq (NExpr a) (NExpr a)
	deriving(Read)
	
data Command a = 
	Assign Ident (NExpr a) | Input Ident | Print Ident | Empty Ident | Push Ident (NExpr a) | Pop Ident Ident | Size Ident Ident |
	Seq [Command a] | Cond (BExpr a) (Command a) (Command a) | Loop (BExpr a) (Command a)
	deriving(Read)

instance Show a => Show (NExpr a) where
	show (Var id) = id
	show (Const value) = show value
	show (Plus x y) = (show x) ++ " + " ++ (show y)
	show (Minus x y) = (show x) ++ " - " ++ (show y)
	show (Times x y) = (show x) ++ " * " ++ (show y)

instance Show a => Show (BExpr a) where
	show (OR x y) = (show x) ++ " OR " ++ (show y)
	show (AND x y) = (show x) ++ " AND " ++ (show y)
	show (NOT x) = "NOT " ++ (show x)
	show (Gt x y) = (show x) ++ " > " ++ (show y)
	show (Eq x y) = (show x) ++ " = " ++ (show y)

instance Show a => Show (Command a) where
	show (Assign id value) = id ++ " := " ++ (show value) ++ "\n"
	show (Input id) = "INPUT " ++ id ++ "\n"
	show (Print id) = "PRINT " ++ id ++ "\n"
	show (Empty id) = "EMPTY " ++ id ++ "\n"
	show (Push p value) = "PUSH " ++ p ++ " " ++ (show value) ++ "\n"
	show (Pop p id) = "POP " ++ p ++ " " ++ id ++ "\n"
	show (Size p id) = "SIZE " ++ p ++ " " ++ id ++ "\n"
	show (Seq commands) = foldr (\a b -> (show a) ++ b) "" commands		
	show (Loop expr seq) = "WHILE " ++ (show expr) ++ "\nDO" ++ (indent ("\n" ++ (show seq))) ++ "END\n"
	show (Cond expr seqIf seqElse) = "IF " ++ (show expr) ++ " THEN" ++ (indent ("\n" ++ (show seqIf))) ++ "ELSE" ++ (indent ("\n" ++ (show seqElse))) ++ "END\n"

indent :: String -> String
indent "" = ""
indent (c:cs) = 
	if (c == '\n') && (cs /= [])
		then "\n  " ++ indent(cs)
		else (c:indent(cs))

-------------------------------------------------------------------------------------------------------------

data Error = Undefined | Stack | Type

throw :: Error -> Either String a
throw Undefined = Left "undefined variable"
throw Stack = Left "empty stack"
throw Type = Left "type error"

type Value a = Either a [a]

data SymTable a = SymTable [(String, Value a)]
	deriving (Show)

getType :: SymTable a -> Ident -> String
getType (SymTable []) _ = "Undefined"
getType (SymTable ((s, v):vs)) id
	| id == s 	= case v of
		Left _ 	-> "Numerical"
		Right _	-> "Stack"
	| otherwise = getType (SymTable vs) id

setValue :: SymTable a -> Ident -> Value a -> Either String (SymTable a)
setValue (SymTable []) id value = Right (SymTable [(id, value)])
setValue (SymTable ((s, elem):elems)) id value
	| id /= s 	= fmap (\(SymTable elemsMod) -> SymTable ((s, elem):elemsMod)) (setValue (SymTable elems) id value)
	| otherwise = case elem of
		(Right _)	-> case value of
			(Right v) 	-> Right (SymTable ((id, Right v):elems))
			_			-> throw Type
		(Left _)	-> case value of
			(Left v)	-> Right (SymTable ((id, Left v):elems))
			_			-> throw Type

getValue :: SymTable a -> Ident -> Maybe (Value a)
getValue (SymTable []) id = Nothing
getValue (SymTable ((s, v):vs)) id
	| id == s 	= Just v
	| otherwise = getValue (SymTable vs) id

getNum :: SymTable a -> Ident -> Maybe a
getNum t id = case (getValue t id) of
	Just (Left value)	-> Just value
	_					-> Nothing

getStack :: SymTable a -> Ident -> Either String [a]
getStack t id = case (getValue t id) of
	Just (Right value)	-> Right value
	Just (Left _)		-> throw Type
	Nothing				-> throw Undefined

-------------------------------------------------------------------------------------------------------------

evaluate :: (Evaluable e, Num a, Ord a) => SymTable a -> e a -> (Either String a)
evaluate t expr = if typeCheck (getType t) expr
	then eval (getNum t) expr
	else throw Undefined

i_eval :: Either String a -> (a -> a -> b) -> (Either String a) -> (Either String b)
i_eval (Left error) op _ = Left error
i_eval _ op (Left error) = Left error
i_eval (Right x) op (Right y) = Right (op x y)

boolEval :: (Num a, Eq a) => (Either String a) -> (Bool -> Bool -> Bool) -> (Either String a) -> (Either String a)
boolEval (Left error) _ _ = Left error
boolEval _ _ (Left error) = Left error
boolEval (Right x) op (Right y) = if op (x /= 0) (y /= 0)
	then Right 1
	else Right 0

class Evaluable e where
	eval :: (Num a, Ord a) => (Ident -> Maybe a) -> (e a) -> (Either String a)
	typeCheck :: (Ident -> String) -> (e a) -> Bool

instance Evaluable NExpr where
	typeCheck f (Const _) = True
	typeCheck f (Var id) = f id == "Numerical"
	typeCheck f (Plus x y) = (typeCheck f x) && (typeCheck f y)
	typeCheck f (Minus x y) = (typeCheck f x) && (typeCheck f y)
	typeCheck f (Times x y) = (typeCheck f x) && (typeCheck f y)

	eval f (Plus x y) = i_eval (eval f x) (+) (eval f y)
	eval f (Minus x y) = i_eval (eval f x) (-) (eval f y)
	eval f (Times x y) = i_eval (eval f x) (*) (eval f y)
	eval f (Const value) = Right value
	eval f (Var id) = case f id of
		Nothing 	-> throw Undefined
		Just value 	-> Right value

instance Evaluable BExpr where
	typeCheck f (OR x y) = (typeCheck f x) && (typeCheck f y)
	typeCheck f (AND x y) = (typeCheck f x) && (typeCheck f y)
	typeCheck f (NOT x) = (typeCheck f x)
	typeCheck f (Gt x y) = (typeCheck f x) && (typeCheck f y)
	typeCheck f (Eq x y) = (typeCheck f x) && (typeCheck f y)

	eval f (AND x y) = fmap (fromBool) (i_eval (eval f x) (\x y -> (x /= 0) && (y /= 0)) (eval f y))
	eval f (OR x y) = fmap (fromBool) (i_eval (eval f x) (\x y -> (x /= 0) || (y /= 0)) (eval f y))
	eval f (NOT x) = fmap (\value -> fromBool (value == 0)) (eval f x)
	eval f (Eq x y) = fmap (fromBool) (i_eval (eval f x) (==) (eval f y))
	eval f (Gt x y) = fmap (fromBool) (i_eval (eval f x) (>) (eval f y))

fromBool :: Num a => Bool -> a
fromBool b = if b then 1 else 0

-------------------------------------------------------------------------------------------------------------

generateRandom :: Num a => a
generateRandom = 4
--generateRandom :: IO a
--generateRandom = randomRIO (-1000, 1000)


type Result a = ((Either String [a]), SymTable a, [a])

interpretCommand :: (Num a, Ord a) => SymTable a -> [a] -> Command a -> Result a

interpretCommand t input (Seq []) = (Right [], t, input)
interpretCommand t1 input1 (Seq (c:cs)) = concatResults resultAct (interpretCommand t2 input2 (Seq cs))
	where
		resultAct = interpretCommand t1 input1 c
		(_, t2, input2) = resultAct
		concatResults :: Result a -> Result a -> Result a
		concatResults (Left error, t, input) _ = (Left error, t, input)
		concatResults _ (Left error, t, input) = (Left error, t, input)
		concatResults (Right output1, _, _) (Right output2, t, input) = (Right (output1 ++ output2), t, input)
		
interpretCommand t [] (Input c) = interpretCommand t [generateRandom] (Input c)
interpretCommand t1 (x:xs) (Input id) = case setValue t1 id (Left x) of
	Left error 	-> (Left error, t1, xs)
	Right t2 	-> (Right [], t2, xs)

interpretCommand t input (Print id) = case getValue t id of
	Nothing 		-> (throw Undefined, t, input)
	Just (Right _)	-> (throw Type, t, input)
	Just (Left x) 	-> (Right [x], t, input)

interpretCommand t1 input (Assign id expr) = case evaluate t1 expr of
	Left error 	-> (Left error, t1, input)
	Right value -> case setValue t1 id (Left value) of
		Left error 	-> (Left error, t1, input)
		Right t2 	-> (Right [], t2, input)

interpretCommand t input (Cond cond seqIf seqElse) = case evaluate t cond of
	Left error 	-> (Left error, t, input)
	Right res 	-> if res /= 0 
		then interpretCommand t input seqIf 
		else interpretCommand t input seqElse

interpretCommand t1 input1 (Loop cond (Seq seq)) = case evaluate t1 cond of
	Left error 	-> (Left error, t1, input1)
	Right value -> if value /= 0
		then interpretCommand t1 input1 (Seq (seq ++ [(Loop cond (Seq seq))]))
		else (Right [], t1, input1) 

interpretCommand t1 input (Empty id) = case setValue t1 id (Right []) of
	Left error 	-> (Left error, t1, input)
	Right t2	-> (Right [], t2, input)

interpretCommand t1 input (Push id expr) = case getStack t1 id of
	Left error 	-> (Left error, t1, input)
	Right p		-> case evaluate t1 expr of
		Left error 	-> (Left error, t1, input)
		Right value -> (Right [], t2, input) 
			where
				Right t2 = setValue t1 id (Right (value:p))

interpretCommand t1 input (Pop p id) = case getStack t1 p of
	Left error 		-> (Left error, t1, input)
	Right (x:xs)	-> case setValue t1 id (Left x) of
		Left error 	-> (Left error, t1, input)
		Right t2 	-> case setValue t2 p (Right xs) of
			Left error 	-> (Left error, t2, input)
			Right t3	-> (Right [], t3, input)

interpretCommand t1 input (Size id1 id2) = case getStack t1 id1 of
	Left error 	-> (Left error, t1, input)
	Right p 	-> case setValue t1 id2 (Left (length p)) of
		Left error 	-> (Left error, t1, input)
		Right t2 	-> (Right [], t2, input)
	where
		length :: Num a => [a] -> a
		length [] = 0
		length (x:xs) = 1 + length xs

interpretProgram :: (Num a, Ord a) => [a] -> Command a -> (Either String [a])
interpretProgram input commands = result
	where
		(result, _, _) = interpretCommand (SymTable []) input commands

{-
getProgram :: IO String
getProgram = readFile "./programhs.txt"

readProgram :: Read a => Int -> IO (Command a)
readProgram 0 = do
	l <- getProgram
	return (read l :: Command Int)
--readProgram 0 = (getProgram >>= read)::IO (Command Int)
--readProgram 1 = getProgram >>= read::IO (Command Double)

main = do
	l <- getLine
	let typeOption = read l
	program <- readProgram (typeOption)
	print program
-}

test :: Command a
test = (Input "hola")::Command Int

{-
getProgram :: Read a => Int -> String -> IO (Command a)
getProgram 0 l = return (read(l)::Command Int)

main = do
	l <- getLine
	o <- getLine
	p <- getProgram (read o) l
	print p
-}
