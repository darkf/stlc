module Term where

data Type = BottomTy
		  | IntTy
		  | ArrowTy Type Type
	deriving (Show, Eq)

data Term = Const Integer
		  | Var String
		  | Abs String Type Term
		  | App Term Term
	deriving (Show, Eq)