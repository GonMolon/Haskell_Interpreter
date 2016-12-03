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

{-
insertIndent :: Int -> String
insertIndent level
	| level == 0 	= ""
	| otherwise 	= "  " ++ (insertIndent (level - 1))

indent :: (Command a) -> Int -> String
indent (Seq (c:cs)) level = (insertIndent level) ++ 
-}

instance (Show a) => Show (Command a) where
	show (Assign var value) = (id var) ++ " := " ++ (show value) ++ "\n"
	show (Input var) = "INPUT " ++ (id var) ++ "\n"
	show (Print var) = "PRINT " ++ (id var) ++ "\n"
	show (Empty var) = "EMPTY " ++ (id var) ++ "\n"
	show (Push p value) = "PUSH " ++ (show p) ++ (show value) ++ "\n"
	show (Pop p var) = "POP " ++ (show p) ++ (id var) ++ "\n"
	show (Size p var) = "SIZE " ++ (show p) ++ (id var) ++ "\n"
	show (Seq commands) = foldr (\a b -> b ++ (show a)) "" commands
		--where indent s = map (\x -> if x == "\n" then "\n  ") 
	--show (Loop expr inst) = "WHILE " ++ expr ++ "\n" ++ "DO\n" ++ (indent inst 0) ++ "END\n"
	--show (Cond expr instIf instElse) = 
