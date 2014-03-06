import Data.Maybe (fromJust)
import Term
import Parser (parseProgram)

typeof :: [(String, Type)] -> Term -> Type
typeof _ Const{} = IntTy
typeof e (Var x) = fromJust (lookup x e)
typeof e (Abs arg ty t) = ArrowTy ty (typeof ((arg, ty):e) t)
typeof e (App t arg) =
	let ArrowTy ty ret = typeof e t in
	if typeof e arg /= ty then
		error "argument type mismatch"
	else ret

substitute :: String -> Term -> Term -> Term
substitute v x (Var v') | v' == v = x
substitute v x (Abs arg ty t) = Abs arg ty (substitute v x t)
substitute v x (App t arg) = App (substitute v x t) (substitute v x arg)
substitute _ _ t = t

eval :: [(String, Term)] -> Term -> Term
eval _ (Const x) = Const x
eval _ x@Abs{} = x
eval e (Var x) = Const (let Const v = fromJust (lookup x e) in v)
eval e (App (Abs arg _ t) x) = eval e (substitute arg x t)
eval e (App t x) = eval e (App (eval e t) x)

fromLeft (Right x) = x
fromLeft (Left err) = error (show err)
main = interact $ show . eval [] . fromLeft . parseProgram