{-
  FIRST -> I need to prove that is well-typed :: EVAL
  SECON -> Reduce reduce reduce               :: REDUCE and CBV

  "The presence of types does not alter the evaluation of an expression at all. So what use are types?"
-}

module Resolve ( evaluate, test, main ) where
import Data.List

data Stlc
  = TVar Char
  | TAbs Char Type Stlc
  | TApp Stlc Stlc
  | TAdd Stlc Stlc
  | TFromFloat Stlc
  | TFromInt Stlc
-----------
  | TUnit
  | TInt Int
  | TFloat Float
  deriving (Show, Eq)

data Type = TypeInt
            | TypeFloat
            | TypeUnit
            | TypeFunction Type Type 
            deriving (Eq, Show)

data MaybeTerm = Message String | Term (Stlc, Int)
instance Show MaybeTerm where
  show (Message s)          = "Message :: " ++ s
  show (Term (term, steps)) = "Term Resolved :: " ++ show term ++ " ;; Steps Left :: " ++ show steps

fromJust :: Maybe a -> a
fromJust (Just x) = x

-- To type check an expression e, we attempt to construct a derivation of the judgment ⊢ e : τ, for some type τ
data Context = Env Char Type deriving Show

addType :: Char -> Type -> [Context] -> [Context]
addType = ((:) .) . f where f a b = Env a b
 
getType :: Char -> [Context] -> Maybe Type
getType _ [] = Nothing
getType x ((Env v t) : context) = if x == v then Just t else getType x context

eval :: [Context] -> Stlc -> Maybe Type
eval c (TAbs n t term) =
    let 
        new = addType n t c
        t'  = eval new term 
    in
      case t' of
        Just tt -> Just $ TypeFunction t tt
        _       -> Nothing
eval c (TApp t t')   =
    let 
        t1 = eval c t
        t2 = eval c t' 
    in
      case t1 of
        Just (TypeFunction a b) -> if Just a == t2 then Just b else Nothing
        _                       -> Nothing
eval c (TAdd t t') =
    let 
        t1 = eval c t
        t2 = eval c t' 
    in
      case t1 of
        Just TypeInt -> 
          case t2 of
            Just TypeInt -> Just TypeInt
            _            -> Nothing
        Just TypeFloat ->
          case t2 of
            Just TypeFloat -> Just TypeFloat
            _              -> Nothing
        _ -> Nothing
eval c (TVar n)   = getType n c
eval _ (TInt i)   = Just TypeInt
eval _ (TFloat f) = Just TypeFloat
eval _ (TUnit)    = Just TypeUnit
eval c (TFromFloat term) = 
  case eval c term of
    Just TypeFloat -> Just TypeInt
    _              -> Nothing
eval c (TFromInt term) = 
  case eval c term of
    Just TypeInt -> Just TypeFloat
    _              -> Nothing

-- Reduce
{-
  Thanks to the definition of E, 
  --> for Application we've got 3 definitions:
  E x, x E and beta-reduction

  --> for Addition we've got 3 definitions:
  E + x, x + E, and x + y (like beta-reduction), we fully reduce
-}

beta :: Stlc -> Stlc -> Stlc
beta (TAbs x tp term) v  = case term of
  TVar t'        -> if t' == x then v else TVar t'
  TAbs x' tp' t' -> if x' == x then TAbs x' tp' t' else TAbs x' tp (beta (TAbs x tp' t') v)
  TApp t t'    -> TApp (beta (TAbs x tp t) v) (beta (TAbs x tp t') v)
  TAdd t t'    -> TAdd (beta (TAbs x tp t) v) (beta (TAbs x tp t') v)
  other        -> other
  
cbv :: Stlc -> Int -> Stlc
cbv term 0 = term
-- Application
cbv (TApp lambda@(TAbs _ _ _) term) c = cbv (beta lambda term) (c-1)
cbv (TApp app@(TApp _ _) term) c    = let app' = cbv app c in cbv (TApp app' term) (c-1)
cbv (TApp term app@(TApp _ _)) c    = let app' = cbv app c in cbv (TApp term app') (c-1)
-- Adition
cbv (TAdd (TInt n) (TInt n')) c     = TInt . uncurry (+) $ (n::Int, n'::Int)
cbv (TAdd i@(TInt n) term) c        = let term' = cbv term c in cbv (TAdd i term') (c-1)
cbv (TAdd (TFloat n) (TFloat n')) c = TFloat . uncurry (+) $ (n::Float, n'::Float)
cbv (TAdd i@(TFloat n) term) c      = let term' = cbv term c in cbv (TAdd i term') (c-1)
cbv (TAdd term i) c                 = let term' = cbv term c in cbv (TAdd term' i) (c-1)
-- Rest
cbv (TFromFloat term) c = TInt . round . getv . uncurry cbv $ (term, c-1) where
                          getv (TFloat f) = f
cbv (TFromInt term) c   = TFloat . precise . getv . uncurry cbv $ (term, c-1) where
                          getv (TInt f) = f
                          precise (n::Int) = fromIntegral n :: Float
cbv x c                 = x

reduce :: Stlc -> Int -> Int -> MaybeTerm
reduce term 0 init = if (cbv term 1) == term then Term (term, 0) else Message . (++) ((++) "Term " $ show term) $ ((++) " might not be totally resolved in " $ show init)
reduce term n init = if (cbv term 1) == term then Term (term, n) else reduce (cbv term 1) (n-1) init

evaluate :: Stlc -> Int -> MaybeTerm
evaluate term n = case eval [] term of
                  Nothing    -> Message . (++) (show term) $ " term does not type check"
                  Just t     -> reduce term n n

-- TEST CASES --
test1 = TApp (TApp (TAbs 'x' TypeInt (TAbs 'y' TypeInt (TVar 'x'))) (TInt 1)) (TInt 2) -- \x\y -> x returns first  :: 1
test2 = TApp (TApp (TAbs 'x' TypeInt (TAbs 'y' TypeInt (TVar 'y'))) (TInt 1)) (TInt 2) -- \x\y -> y returns second :: 2
test3 = TAdd (TInt 4) (TInt 3) -- Simple Addition
test4 = TApp (TAbs 'x' TypeInt (TAdd (TVar 'x') (TInt 4))) (TInt 2) -- Addition with App: 
test5 = TApp (TAbs 'x' TypeFloat (TAdd (TVar 'x') (TFloat 1.2))) (TFromInt (TInt 2)) -- Same as 3 but with Floats and FromInt
test6 = TApp (TAbs 'x' TypeInt (TAdd (TApp (TAbs 'x' TypeInt (TAdd (TVar 'x') (TInt 1))) (TVar 'x')) (TInt 1))) (TInt 0) -- f . f $ x where f = (+1) [IMPLICIT] ==> f ( f ( 0 )) = 1
test7 = TApp (TApp (TAbs 'f' (TypeFunction TypeInt TypeInt) (TAbs 'x' TypeInt (TApp (TVar 'f') (TApp (TVar 'f') (TVar 'x'))))) ((TAbs 'x' TypeInt (TAdd (TVar 'x') (TInt 1))))) (TInt 0) -- f . f $ x where f = (+1) but now giving f as argument ==> \f\x -> f(f(x))

test :: Stlc -> Int -> String
test = (show .) . evaluate

-- Executing main, the result from Test 5 is the same as Test 6, but for Test 6, not sufficient steps were provided
tests = [test test1 3, test test2 3, test test3 1, test test4 3,  test test5 3, test test5 2, test test6 4, test test7 6]

-- RUN MAIN ==> RUN ALL THE TESTS IN tests
main :: IO ()
main = putStrLn . ((++) "\n") . concat . reverse . f $ tests where
    f = foldl (\x xs -> (:) ((++) ((++) (show . succ . length $ x) " -> ") xs ++ "\n") x) []
-- TEST CASES --