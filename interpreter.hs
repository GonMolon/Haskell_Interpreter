{-# LANGUAGE FlexibleInstances #-}

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

instance (Show a) => Show (NExpr a) where
	show (Var id) = id
	show (Const value) = show value
	show (Plus x y) = (show x) ++ " + " ++ (show y)
	show (Minus x y) = (show x) ++ " - " ++ (show y)
	show (Times x y) = (show x) ++ " * " ++ (show y)

instance (Show a) => Show (BExpr a) where
	show (OR x y) = (show x) ++ " OR " ++ (show y)
	show (AND x y) = (show x) ++ " AND " ++ (show y)
	show (NOT x) = "NOT " ++ (show x)
	show (Gt x y) = (show x) ++ " > " ++ (show y)
	show (Eq x y) = (show x) ++ " = " ++ (show y)

instance (Show a) => Show (Command a) where
	show (Assign id value) = id ++ " := " ++ (show value) ++ "\n"
	show (Input id) = "INPUT " ++ id ++ "\n"
	show (Print id) = "PRINT " ++ id ++ "\n"
	show (Empty id) = "EMPTY " ++ id ++ "\n"
	show (Push p value) = "PUSH " ++ p ++ " " ++ (show value) ++ "\n"
	show (Pop p id) = "POP " ++ p ++ " " ++ id ++ "\n"
	show (Size p id) = "SIZE " ++ p ++ " " ++ id ++ "\n"
	show (Seq commands) = foldr (\a b -> (show a) ++ b) "" commands		
	show (Loop expr inst) = "WHILE " ++ (show expr) ++ "\nDO" ++ (indent ("\n" ++ (show inst))) ++ "END\n"
	show (Cond expr instIf instElse) = "IF " ++ (show expr) ++ " THEN" ++ (indent ("\n" ++ (show instIf))) ++ "ELSE" ++ (indent ("\n" ++ (show instElse))) ++ "END\n"

indent :: String -> String
indent "" = ""
indent (c:cs) = 
	if (c == '\n') && (cs /= [])
		then "\n  " ++ indent(cs)
		else (c:indent(cs))

data Error = Undefined | Stack | Type

throw :: Error -> Either String a
throw Undefined = Left "undefined variable"
throw Stack = Left "empty stack"
throw Type = Left "type error"

type Value a = Either a [a]

data SymTable a = SymTable [(String, Value a)]

setValue :: SymTable a -> Ident -> Value a -> Either String (SymTable a)
setValue (SymTable []) id v = Right (SymTable [(id, v)])
setValue (SymTable ((s, (Left v)):vs)) id (Left value)
	| id == s 	= Right (SymTable ((s, (Left value)):vs))
	| otherwise	= setValue (SymTable vs) id (Left value)
setValue (SymTable ((s, (Right v)):vs)) id (Left value)
	| id == s 	= throw Type
	| otherwise = setValue (SymTable vs) id (Left value)
setValue (SymTable ((s, (Right v)):vs)) id (Right value)
	| id == s 	= Right (SymTable ((s, (Right value)):vs))
	| otherwise = setValue (SymTable vs) id (Right value)
setValue (SymTable ((s, (Left v)):vs)) id (Right value)
	| id == s 	= throw Type
	| otherwise	= setValue (SymTable vs) id (Right value)

getValue :: SymTable a -> Ident -> Maybe (Value a)
getValue (SymTable []) id = Nothing
getValue (SymTable ((s, v):vs)) id
	| id == s 	= Just v
	| otherwise = getValue (SymTable vs) id

getNum :: SymTable a -> Ident -> Maybe a
getNum t id = case (getValue t id) of
	(Just (Left a))	-> Just a
	_				-> Nothing

numEval :: (Either String a) -> (a -> a -> a) -> (Either String a) -> (Either String a)
numEval (Left error) op _ = Left error
numEval _ op (Left error) = Left error
numEval (Right x) op (Right y) = Right (op x y)

boolEval :: (Num a, Eq a) => (Either String a) -> (Bool -> Bool -> Bool) -> (Either String a) -> (Either String a)
boolEval (Left error) op _ = Left error
boolEval _ op (Left error) = Left error
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

	eval f (Const value) = Right value
	eval f (Var id) = case f id of
		Nothing 	-> throw Undefined
		Just value 	-> Right value
	eval f (Plus x y) = numEval (eval f x) (+) (eval f y)
	eval f (Minus x y) = numEval (eval f x) (-) (eval f y)
	eval f (Times x y) = numEval (eval f x) (*) (eval f y)

instance Evaluable BExpr where
	typeCheck f (OR x y) = (typeCheck f x) && (typeCheck f y)
	typeCheck f (AND x y) = (typeCheck f x) && (typeCheck f y)
	typeCheck f (NOT x) = (typeCheck f x)
	typeCheck f (Gt x y) = (typeCheck f x) && (typeCheck f y)
	typeCheck f (Eq x y) = (typeCheck f x) && (typeCheck f y)


	eval f (OR x y) = boolEval (eval f x) (||) (eval f y)
	eval f (AND x y) = boolEval (eval f x) (&&) (eval f y)
	eval f (Eq x y) = boolEval (eval f x) (==) (eval f y)
	eval f (Gt x y) = boolEval (eval f x) (>) (eval f y)
	eval f (NOT x) = case eval f x of
		(Left error) 	-> Left error
		(Right value)	-> Right (if value == 0 then 1 else 0)


random :: Num a => a
random = 4

type Result a = ((Either String [a]), SymTable a, [a])

concatResults :: Result a -> Result a -> Result a
concatResults (Left error, t, input) _ = (Left error, t, input)
concatResults _ (Left error, t, input) = (Left error, t, input)
concatResults (Right output1, _, _) (Right output2, t, input) = (Right (output1 ++ output2), t, input)

interpretCommand :: (Num a, Ord a) => SymTable a -> [a] -> Command a -> Result a

interpretCommand t [] c = interpretCommand t [random] c

interpretCommand t input (Seq []) = (Right [], t, input)
--TODO test if this is efficient by: incorrect command; infinite loop
interpretCommand t input (Seq (c:cs)) = concatResults (interpretCommand t input c) (interpretCommand t input (Seq cs))
		

interpretCommand t input (Loop cond seq) = case eval (getNum t) cond of
	Left error -> (Left error, t, input)
	Right value -> if value == 0
		then (Right [], t, input)
		else concatResults resultSeq (interpretCommand t input2 (Loop cond seq))
		where
			resultSeq = interpretCommand t input seq
			(_, t, input2) = resultSeq

{-
INPUT X
INPUT Y 
OUTPUT Y
WHILE X < Y DO 
  OUTPUT X
  y := Y + 1
 END
-}

--interpretCommand t (x:xs) (Input id) = 
--	where
--		input :: ((Either String [a]), SymTable a, [a])

interpretProgram :: (Num a, Ord a) => [a] -> Command a -> (Either String [a])
interpretProgram input commands = result
	where
		(result, _, _) = interpretCommand (SymTable []) input commands
