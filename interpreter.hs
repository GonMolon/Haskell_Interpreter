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
	show (Var var) = id var
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
	show (Assign var value) = (id var) ++ " := " ++ (show value) ++ "\n"
	show (Input var) = "INPUT " ++ (id var) ++ "\n"
	show (Print var) = "PRINT " ++ (id var) ++ "\n"
	show (Empty var) = "EMPTY " ++ (id var) ++ "\n"
	show (Push p value) = "PUSH " ++ (id p) ++ " " ++ (show value) ++ "\n"
	show (Pop p var) = "POP " ++ (id p) ++ " " ++ (id var) ++ "\n"
	show (Size p var) = "SIZE " ++ (id p) ++ " " ++ (id var) ++ "\n"
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
getNum t id = extractNum (getValue t id)
	where
		extractNum :: (Maybe (Value a)) -> Maybe a
		extractNum Nothing = Nothing
		extractNum (Just (Right p)) = Nothing
		extractNum (Just (Left a)) = Just a

class Evaluable e where
	eval :: (Num a, Ord a) => (Ident -> Maybe a) -> (e a) -> (Either String a)
	--typeCheck :: (Ident -> String) -> (e a) -> Bool
{-
instance Evaluable NExpr where
	eval f (Const value) = Right value
	eval f (Var id) = 

instance Evaluable BExpr where
	eval f (OR x y) = (eval f x) || (eval f y)
	eval f (Gt x y) = (eval f x) < (eval f y)

eval :: NExpr a -> (Either Stirng a)
eval expr = eval getNum expr

eval :: BExpr

input :: 
-}


interpretCommand :: (Num a, Ord a) => SymTable a -> [a] -> Command a -> ((Either String [a]), SymTable a, [a])
interpretCommand t input (Seq []) = (Right [], t, input)
interpretCommand t input (Seq (c:cs)) = processResult (interpretCommand t input c) cs
	where
		processResult :: (Num a, Ord a) => ((Either String [a]), SymTable a, [a]) -> [Command a] -> ((Either String [a]), SymTable a, [a])
		processResult (Left error, t, input) _ = (Left error, t, input)
		processResult (Right output, t, input) cs = concatResult output (interpretCommand t input (Seq cs))

		concatResult :: (Num a, Ord a) => [a] -> ((Either String [a]), SymTable a, [a]) -> ((Either String [a]), SymTable a, [a])
		concatResult _ (Left error, t, input) = (Left error, t, input)
		concatResult output1 (Right output2, t, input) = (Right (output1 ++ output2), t, input)
interpretCommand t (x:xs) (Input id) = (Left "hello", t, (x:xs))--input (setValue t id x)
--	where
--		input :: ((Either String [a]), SymTable a, [a])

interpretProgram :: (Num a, Ord a) => [a] -> Command a -> (Either String [a])
interpretProgram input commands = first (interpretCommand (SymTable []) input commands)
	where
		first :: ((Either String [a]), SymTable a, [a]) -> (Either String [a])
		first (result, _, _) = result



