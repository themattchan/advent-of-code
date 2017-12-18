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
import Debug.Trace
import Data.Functor.Identity
import qualified Data.Map.Strict as M
import Control.Concurrent.STM.TChan

data DuetState = DS
  { registersDS :: M.Map Char Int
  , lastFreqDS :: Maybe Int
  , messagesInDS :: TChan Int
  , messagesOutDS :: TChan Int
  , commandsDS :: Zipper Command
  }

data End = Recover Int | FinishedCommands deriving Show

type DuetT m a = ExceptT End ((StateT DuetState) m) a
type Duet a = DuetT Identity a

currCommand :: Duet Command
currCommand = gets (viewFoci . commandsDS)

moveCommand :: Int -> Duet ()
moveCommand n = do
  z <- gets commandsDS
  case move n z of
    Nothing -> throwError FinishedCommands
    Just z' -> modify (\ds -> ds { commandsDS = z' })

nextCommand = moveCommand 1

getVal :: Value -> Duet (Maybe Int)
getVal (Val n) = pure (Just n)
getVal (Reg c) = gets (M.lookup c . registersDS)

modifyReg :: Reg -> Maybe (Int -> Int) -> Duet ()
modifyReg r Nothing = pure ()
modifyReg r (Just f) = do
  modify (\ds -> ds {registersDS = M.adjust f r (registersDS ds)})

setReg :: Reg -> Int -> Duet ()
setReg r i = modify (\ds -> ds {registersDS = M.insert r i (registersDS ds)})

solve1 :: Zipper Command -> Either End ()
solve1 = evalState (runExceptT run) . DS mempty Nothing undefined undefined
  where
    run = forever $ currCommand >>= interpCommand1

    interpCommand1 = \case
      Snd v -> getVal v >>= \v' -> modify' (\ds -> ds {lastFreqDS = v' }) >> nextCommand
      Op op r v -> ($ op) $ \case
        Set -> getVal v >>= setReg r . fromJust >> nextCommand
        Add -> (fmap (+) <$> getVal v) >>= modifyReg r >> nextCommand
        Mul -> (fmap (*) <$> getVal v) >>= modifyReg r >> nextCommand
        Mod -> (fmap (flip mod) <$> getVal v) >>= modifyReg r >> nextCommand
      Rcv v -> do
        val <- getVal v
        lf <- gets lastFreqDS
        if isJust val && fromJust val /= 0 && isJust lf
          then throwError $ Recover (fromJust lf)
          else nextCommand

      Jgz cond off -> do
        condv <- getVal cond
        offv <- getVal off
        if isJust condv && isJust offv && fromJust condv > 0
          then moveCommand (fromJust offv)
          else nextCommand

-- solve2 :: IO Int
-- solve2 = do
--   chan0 :: TChan Int <- newTChanIO
--   chan1 :: TChan Int <- newTChanIO


type Reg = Char
data Value = Val Int | Reg Reg deriving Show
data Op = Set | Add | Mul | Mod deriving Show
data Command = Snd Value | Op Op Reg Value | Rcv Value | Jgz Value Value deriving Show

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
    pRcv = do string "rcv"; spaces; Rcv <$> value
    pJgz = do string "jgz"; spaces; Jgz <$> (value <* spaces) <*> value

main :: IO ()
main = do
  Just commands <- runParser parseCommands <$> readFile "input.txt"
  print $ solve1 commands
