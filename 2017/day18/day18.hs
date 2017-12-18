#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Utils
import Zipper
import Control.Monad.State.Lazy
import Control.Monad.Except
import Data.Functor.Identity
import qualified Data.Map.Strict as M

type Reg = Char
data Value = Val Int | Reg Reg deriving Show
data Op = Set | Add | Mul | Mod deriving Show
data Command = Snd Value | Op Op Reg Value | Rcv Reg | Jgz Value Value deriving Show

parseCommands :: Parser (Zipper Command)
parseCommands = zipperFromList <$> many parseOne
  where
    parseOne = foldl1 (<|>) [pSnd, pSet, pAdd, pMul, pMod, pRcv, pJgz] <* spaces

    reg = take1
    value = (Val <$> num) <|> (Reg <$> reg)
    pOp op = Op op <$> (reg <* spaces) <*> value

    pSnd = do string "snd"; spaces; Snd <$> value
    pSet = do string "set"; spaces; pOp Set
    pAdd = do string "add"; spaces; pOp Add
    pMul = do string "mul"; spaces; pOp Mul
    pMod = do string "mod"; spaces; pOp Mod
    pRcv = do string "rcv"; spaces; Rcv <$> reg
    pJgz = do string "jgz"; spaces; Jgz <$> (value <* spaces) <*> value

data DuetState = DS
  { registersDS :: M.Map Char Int
  , lastFreqDS :: Maybe Int
  , commandsDS :: Zipper Command
  }

data End = Recovered Int | FinishedCommands deriving Show

type DuetT m a = ExceptT End ((StateT DuetState) m) a
type Duet a = DuetT Identity a

currCommand :: Monad m => DuetT m Command
currCommand = gets (viewFoci . commandsDS)

moveCommand :: Monad m => Int -> DuetT m ()
moveCommand n = do
  z <- gets commandsDS
  case move n z of
    Nothing -> throwError FinishedCommands
    Just z' -> modify (\ds -> ds { commandsDS = z' })

nextCommand :: Monad m => DuetT m ()
nextCommand = moveCommand 1

getVal :: Monad m => Value -> DuetT m (Maybe Int)
getVal (Val n) = pure (Just n)
getVal (Reg c) = gets (M.lookup c . registersDS)

modifyReg :: Monad m => Reg -> Maybe (Int -> Int) -> DuetT m ()
modifyReg r Nothing = pure ()
modifyReg r (Just f) = do
  modify (\ds -> ds {registersDS = M.adjust f r (registersDS ds)})

setReg :: Monad m => Reg -> Int -> DuetT m ()
setReg r i = modify (\ds -> ds {registersDS = M.insert r i (registersDS ds)})

fromLeft (Left x) = x

solve1 :: Zipper Command -> End
solve1 = fromLeft . evalState (runExceptT run) . DS mempty Nothing
  where
    run = forever $ currCommand >>= interpCommand

    interpCommand = \case
      Snd v -> getVal v >>= \v' -> modify' (\ds -> ds {lastFreqDS = v' }) >> nextCommand

      Op op r v -> op & \case
        Set -> getVal v >>= setReg r . fromJust >> nextCommand
        Add -> (fmap (+) <$> getVal v) >>= modifyReg r >> nextCommand
        Mul -> (fmap (*) <$> getVal v) >>= modifyReg r >> nextCommand
        Mod -> (fmap (flip mod) <$> getVal v) >>= modifyReg r >> nextCommand

      Rcv v -> do
        val <- getVal (Reg v)
        lf <- gets lastFreqDS
        if isJust val && fromJust val /= 0 && isJust lf
          then throwError $ Recovered (fromJust lf)
          else nextCommand

      Jgz cond off -> do
        condv <- getVal cond
        offv <- getVal off
        if isJust condv && isJust offv && fromJust condv > 0
          then moveCommand (fromJust offv)
          else nextCommand

-- for two coordinating programs, translate to CPS and step through.

data DuetC = Send Int DuetC
           | Receive (Int -> DuetC)
           | Finished

getVal' (Val n) _ = n
getVal' (Reg r) st = st M.! r

reify :: Zipper Command -> M.Map Char Int -> DuetC
reify = go . Just
  where
    go Nothing st = Finished
    go (Just z) st = viewFoci z & \case
      Snd v -> Send (getVal' v st) (go (move 1 z) st)
      Rcv r -> Receive (\v -> go (move 1 z) (M.insert r v st))
      Op op r v -> op & \case
        Set -> go (move 1 z) (M.insert r (getVal' v st) st)
        Add -> go (move 1 z) (M.alter (fmap (+ getVal' v st)) r st)
        Mul -> go (move 1 z) (M.alter (fmap (* getVal' v st)) r st)
        Mod -> go (move 1 z) (M.alter (fmap (`mod` getVal' v st)) r st)

      Jgz cond off | getVal' cond st > 0 -> go (move (getVal' off st) z) st
                   | otherwise           -> go (move 1 z) st

solve2 commands = go zero one [] [] 0
  where
    zero = reify commands (M.singleton 'p' 0)
    one  = reify commands (M.singleton 'p' 1)

    go (Send i zk) ok zq oq c = go zk ok zq (oq ++ [i]) c
    go zk (Send i ok) zq oq c = go zk ok (zq ++ [i]) oq (c+1)
    go (Receive zk) ok (i:zq) oq c = go (zk i) ok zq oq c
    go zk (Receive ok) zq (i:oq) c = go zk (ok i) zq oq c
    go _ _ _ _ c = c

main :: IO ()
main = do
  Just commands <- runParser parseCommands <$> readFile "input.txt"
  print $ solve1 commands
  print $ solve2 commands
