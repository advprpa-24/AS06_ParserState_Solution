-------------------------------------------------------------------------------
-- Interpreter for the Imp language
-------------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module Lang.Imp.Interpreter where
import Lang.Imp.Ast
import qualified Data.Map.Strict as M

todo :: a
todo = error "TODO"


type State = M.Map Name Integer

emptyState :: State
emptyState = M.empty


-- ImpExpr: Value which takes a state and either returns a value or an error.
newtype ImpExpr a = ImpExpr { runExpr :: State -> Either String a }


-- The Functor instance for ImpExpr:
instance Functor ImpExpr where
  fmap :: (a -> b) -> ImpExpr a -> ImpExpr b
  fmap f ma = ma >>= pure . f


-- The Applicative instance for ImpExpr:
instance Applicative ImpExpr where
  pure :: a -> ImpExpr a
  pure a = ImpExpr (\_ -> Right a) 

  (<*>) :: ImpExpr (a -> b) -> ImpExpr a -> ImpExpr b
  mf <*> ma = do
   f <- mf
   a <- ma
   pure (f a)


-- The Monad instance for ImpExpr:
instance Monad ImpExpr where
  (>>=) :: ImpExpr a -> (a -> ImpExpr b) -> ImpExpr b
  ImpExpr a >>= f = ImpExpr (\st -> 
    a st >>= \x -> runExpr (f x) st) -- the (>>=) on this line is the one from Either


-- Returns the current state.
ask :: ImpExpr State
ask = ImpExpr $ \st -> Right st


-- Fails with Left and the given error message.
failWith :: String -> ImpExpr a
failWith s = ImpExpr $ \_ -> Left s


-- Reads a variable from the state. Fails with Left if the variable is not found.
readVariable :: Name -> ImpExpr Integer
readVariable n = do
  st <- ask
  maybe (failWith $ "variable not found: " ++ n) pure (M.lookup n st)


eval :: Expr a -> ImpExpr a
eval (BNot b)     = fmap not (eval b)
eval (BAnd a b)   = pure (&&) <*> eval a <*> eval b
eval (BOr a b)    = pure (||) <*> eval a <*> eval b

eval (REq a b)    = pure (==) <*> eval a <*> eval b   
eval (RLt a b)    = pure (<) <*> eval a <*> eval b

eval (AVar n)     = readVariable n
eval (AConst n)   = pure n
eval (APlus a b)  = pure (+) <*> eval a <*> eval b
eval (AMinus a b) = pure (-) <*> eval a <*> eval b  
eval (AMul a b)   = pure (*) <*> eval a <*> eval b
eval (ADiv a b)   = do
  a' <- eval a
  b' <- eval b
  if b' == 0
    then failWith "division by zero"
    else pure (div a' b')
eval (AMod a b)   = pure mod <*> eval a <*> eval b


-- State monad for the Cmd interpreter:
newtype ImpCmd a = ImpCmd { runState :: State -> Either String (State, a)}

instance Functor ImpCmd where
  fmap :: (a -> b) -> ImpCmd a -> ImpCmd b
  fmap f ma = ma >>= pure . f


instance Applicative ImpCmd where
  pure :: a -> ImpCmd a
  pure a = ImpCmd (\st -> Right (st, a)) 
  (<*>) :: ImpCmd (a -> b) -> ImpCmd a -> ImpCmd b
  mf <*> ma = do
    f <- mf
    a <- ma
    pure (f a)


instance Monad ImpCmd where
  (>>=) :: ImpCmd a -> (a -> ImpCmd b) -> ImpCmd b
  ImpCmd a >>= f = ImpCmd (\st -> 
    a st >>= \(st',x) -> runState (f x) st')


-- Returns the current state
getState :: ImpCmd State
getState = ImpCmd $ \st -> Right (st, st)


-- Overwrites the state with the given argument
setState :: State -> ImpCmd ()
setState st = ImpCmd $ \_ -> Right (st, ())


-- Helper to update the current state
modifyState :: (State -> State) -> ImpCmd ()
modifyState f = do
  st <- getState
  setState (f st)


-- Sets the assignment in the state
assignVariable :: Name -> Integer -> ImpCmd ()
assignVariable n v = modifyState (M.insert n v)


-- Lifts an ImpExpr into an ImpCmd
liftReader :: ImpExpr a -> ImpCmd a
liftReader expr = ImpCmd (\st -> fmap (st, ) (runExpr expr st))


-- TODO: Complete the implementation of the `exec` function.
exec :: Cmd -> ImpCmd ()
exec CSkip           = pure ()
exec (CAssign n v)   = do
  value <- liftReader $ eval v
  assignVariable n value
exec (CIfThEl b t e) = do
  c <- liftReader $ eval b
  exec (if c then t else e)
exec (CWhile b c)    = do
  cond <- liftReader $ eval b
  if cond then do
    exec c
    exec (CWhile b c) 
  else pure ()
exec (CSeq cs) = mapM_ exec cs

