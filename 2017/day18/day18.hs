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
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.STM

data DuetState = DS
  { registersDS :: M.Map Char Int
  , lastFreqDS :: Maybe Int
  -- , sendTimesDS :: Int
  -- , resultDS :: MVar Int
  -- , isRecvDS :: MVar ()
  -- , messagesInDS :: TChan Int
  -- , messagesOutDS :: TChan Int
  , commandsDS :: Zipper Command
  }

data End = Part1 Int | FinishedCommands deriving Show

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
solve1 = fromLeft . evalState (runExceptT run) . DS mempty Nothing --0 undefined undefined undefined undefined
  where
    run = forever $ currCommand >>= interpCommand1

interpCommand1 :: Monad m => Command -> DuetT m ()
interpCommand1 = \case
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
      then throwError $ Part1 (fromJust lf)
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
reify z = go (Just z)
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

      Jgz cond off ->
        let condv = getVal' cond st
            offv = getVal' off st
        in if condv > 0
          then go (move offv z) st
          else go (move 1 z) st

solve2 commands = go zero one [] [] 0
  where
    zero = reify commands (M.singleton 'p' 0)
    one  = reify commands (M.singleton 'p' 1)

    go (Send i zk) ok zq oq c = go zk ok zq (oq ++ [i]) c
    go zk (Send i ok) zq oq c = go zk ok (zq ++ [i]) oq (c+1)
    go (Receive zk) ok (i:zq) oq c = go (zk i) ok zq oq c
    go zk (Receive ok) zq (i:oq) c = go zk (ok i) zq oq c
    go _ _ _ _ c = c

-- solve2 :: Zipper Command -> IO Int
-- solve2 commands = do
--   result0 <- newMVar 0
--   result1 <- newMVar 0
--   isRecv0 <- newMVar ()         -- just used to block
--   isRecv1 <- newMVar ()         -- just used to block
--   chan0 :: TChan Int <- newTChanIO
--   chan1 :: TChan Int <- newTChanIO
--   let state0 = DS (M.singleton 'p' 0) Nothing 0 result0 isRecv0 chan0 chan1 commands
--   let state1 = DS (M.singleton 'p' 1) Nothing 0 result1 isRecv1 chan1 chan0 commands
--   let
--     interpCommand2 :: Command -> DuetT IO ()
--     interpCommand2 = \case
--         Snd v -> do
--           v' <- getVal v
--           case v' of
--             Nothing -> nextCommand
--             Just val -> do
--               outChan <- gets messagesOutDS
--               liftIO $ atomically $ writeTChan outChan val
-- --              modify (\ds -> ds {sendTimesDS = 1 + sendTimesDS ds})
--               mv <- gets resultDS
--               r <- liftIO $ takeMVar mv
--               liftIO $ putMVar mv (r+1)
--               nextCommand

--         Rcv r -> do
--           lock <- gets isRecvDS
--           liftIO $ takeMVar lock
--           inChan <- gets messagesInDS
--           outChan <- gets messagesOutDS
--           mv <- gets resultDS
--           sendN <- gets sendTimesDS
--           v <- liftIO $ atomically $ readTChan inChan
--           liftIO $ putMVar lock ()
--           setReg r v

--           nextCommand

-- --          outEmpty <- liftIO $ atomically $ isEmptyTChan outChan
-- --          if outEmpty then
-- --            liftIO $ putMVar mv (Part2 sendN)
-- --          else do
--             -- case input of
--           --   Nothing -> do

--           --   Just v ->
--         x -> interpCommand1 x

--   let run = forever $ currCommand >>= interpCommand2
--   -- let waitFor1 = do {
--   --       b <- isEmptyMVar result1;
--   --       if b
--   --       then do threadDelay 1000; waitFor1
--   --       else return ()
--   --       }
--   let waitForDeadLock = do
--         r0 <- tryReadMVar isRecv0
--         r1 <- tryReadMVar isRecv1
--         t0 <- atomically $ isEmptyTChan chan0
--         t1 <- atomically $ isEmptyTChan chan1
--         if (r0,r1) == (Nothing, Nothing) && t0 && t1 then return ()
--           else threadDelay 10; waitForDeadLock

--   t0 <- forkIO $ void $ evalStateT (runExceptT run) state0
--   t1 <- forkIO $ void $ evalStateT (runExceptT run) state1
--   waitForDeadLock
--   killThread t0
--   killThread t1
--   readMVar result1

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

main :: IO ()
main = do
  Just commands <- runParser parseCommands <$> readFile "input.txt"
  print $ solve1 commands
  print $ solve2 commands
