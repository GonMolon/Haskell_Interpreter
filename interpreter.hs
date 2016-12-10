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
	Assign Ident (NExpr a) | Input Ident | Print (NExpr a) | Empty Ident | Push Ident (NExpr a) | Pop Ident Ident | Size Ident Ident |
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
	show (Assign id expr) = id ++ " := " ++ (show expr) ++ "\n"
	show (Input id) = "INPUT " ++ id ++ "\n"
	show (Print expr) = "PRINT " ++ (show expr) ++ "\n"
	show (Empty id) = "EMPTY " ++ id ++ "\n"
	show (Push p expr) = "PUSH " ++ p ++ " " ++ (show expr) ++ "\n"
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

data Error = Undefined | EmptyStack | TypeError

throw :: Error -> Either String a
throw Undefined = Left "undefined variable"
throw EmptyStack = Left "empty stack"
throw TypeError = Left "type error"

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
			x@(Right v) 	-> Right (SymTable ((id, x):elems))
			_			-> throw TypeError
		(Left _)	-> case value of
			x@(Left v)	-> Right (SymTable ((id, x):elems))
			_			-> throw TypeError

getNum :: SymTable a -> Ident -> Maybe a
getNum (SymTable l) id = case (lookup id l) of
	Just (Left value)	-> Just value
	_					-> Nothing

getStack :: SymTable a -> Ident -> Either String [a]
getStack (SymTable l) id = case (lookup id l) of
	Just (Right value)	-> Right value
	Just (Left _)		-> throw TypeError
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

type Result a = ((Either String [a]), SymTable a, [a])

interpretCommand :: (Num a, Ord a) => SymTable a -> [a] -> Command a -> Result a

interpretCommand t input (Seq []) = (Right [], t, input)
interpretCommand t1 input1 (Seq (c:cs)) = concatResults resultAct (interpretCommand t2 input2 (Seq cs))
	where
		resultAct = interpretCommand t1 input1 c
		(_, t2, input2) = resultAct
		concatResults :: Result a -> Result a -> Result a
		concatResults (e@(Left error), t, input) _ = (e, t, input)
		concatResults _ (e@(Left error), t, input) = (e, t, input)
		concatResults (Right output1, _, _) (Right output2, t, input) = (Right (output1 ++ output2), t, input)
		
interpretCommand t1 (x:xs) (Input id) = case setValue t1 id (Left x) of
	Left error 	-> (Left error, t1, xs)
	Right t2 	-> (Right [], t2, xs)

interpretCommand t input (Print expr) = case evaluate t expr of
	Left error 	-> (Left error, t, input)
	Right value -> (Right [value], t, input)

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
	Right ([])		-> (throw EmptyStack, t1, input)
	Right (x:xs)	-> case setValue t1 id (Left x) of
		Left error 	-> (Left error, t1, input)
		Right t2 	-> case setValue t2 p (Right xs) of
			Left error 	-> (Left error, t2, input)
			Right t3	-> (Right [], t3, input)

interpretCommand t1 input (Size id1 id2) = case getStack t1 id1 of
	Left error 	-> (Left error, t1, input)
	Right p 	-> case setValue t1 id2 (Left (fromInteger (toInteger(length p)))) of
		Left error 	-> (Left error, t1, input)
		Right t2 	-> (Right [], t2, input)

interpretProgram :: (Num a, Ord a) => [a] -> Command a -> (Either String [a])
interpretProgram input commands = result
	where
		(result, _, _) = interpretCommand (SymTable []) input commands

-------------------------------------------------------------------------------------------------------------

readProgram :: Read a => IO (Command a)
readProgram = do
	h <- (openFile "./programhs.txt") ReadMode
	l <- hGetLine h
	let c = (read l)
	hClose h
	return c

printList :: Show a => [a] -> String
printList l = foldl (\a b -> a ++ " " ++ (show b)) "" l

infiniteList :: (Num a, Random a) => Int -> [a]
infiniteList seed = randomRs (-1000, 1000) (mkStdGen seed)

i_executeTests :: (Show a, Num a, Ord a, Random a) => Command a -> Int -> Int -> Int -> IO String
i_executeTests _ 0 _ _ = return ""
i_executeTests program i k seed = do
	let input = infiniteList seed
	let result = case interpretCommand (SymTable []) (input ++ (infiniteList seed)) program of
		(Left error, _, _)			-> "Execution error in test " ++ (show testNum) ++ ": " ++ error
		(Right output, _, unread) 	->
			"Input test " ++ (show testNum) ++ ": " ++ (printList (getInput input unread)) ++ "\n" ++
			"Outut test " ++ (show testNum) ++ ": " ++ (printList output) ++ "\n"
	i_executeTests program (i-1) k (head (infiniteList seed)) >>= (\r -> return (result ++ r))
	where
		testNum = (k - i + 1)
		getInput :: Eq a => [a] -> [a] -> [a]
		getInput (x:xs) unread@(u:us)
			| x == u 	= []
			| otherwise = (x:(getInput xs unread))

executeTests p k seed = i_executeTests p k k seed

execute :: (Show a, Read a, Num a, Ord a, Random a) => (Command a) -> Int -> Int -> IO String
execute program 0 seed = do
	putStrLn ("Insert the input list (separated by spaces)")
	l <- getLine
	let input = map read (words l)
	case interpretProgram (input ++ (infiniteList seed)) program of
		Left error 		-> return ("Execution error: " ++ error)
		Right output 	-> return ("Outut: " ++ (printList output))
execute program 1 seed = executeTests program 1 seed
execute program 2 seed = do
	putStrLn ("How many tests do you want to execute?")
	l <- getLine
	let k = read l
	executeTests program k seed

main = do
	putStrLn ("Select the data type:")
	putStrLn ("   0 : Int")
	putStrLn ("   1 : Double")
	l <- getLine
	let typeOption = read l
	putStrLn ("Insert the execution type:")
	putStrLn ("   0 : Manual")
	putStrLn ("   1 : Test")
	putStrLn ("   2 : Multiple test")
	l <- getLine
	let execType = read l
	putStrLn ("Insert a random seed:")
	l <- getLine
	let seed = read l
	if typeOption == 0
		then do 
			program <- readProgram::IO (Command Int)
			(execute program execType seed) >>= putStrLn
		else do
			program <- readProgram::IO (Command Double)
			(execute program execType seed) >>= putStrLn


