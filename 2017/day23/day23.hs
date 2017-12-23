#! /usr/bin/env runhaskell -i../../
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
import Utils
import Zipper
import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import Control.Monad.Except
import Data.Functor.Identity
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

type Reg = Char
data Value = Val Int | Reg Reg deriving Show
data Op = Set | Sub | Mul deriving Show
data Command = Op Op Reg Value | Jnz Value Value deriving Show

parseCommands :: Parser [Command]
parseCommands = many parseOne
  where
    parseOne = foldl1 (<|>)
      [ do string "set"; spaces; pOp Set
      , do string "sub"; spaces; pOp Sub
      , do string "mul"; spaces; pOp Mul
      , do string "jnz"; spaces; Jnz <$> (value <* spaces) <*> value
      ] <* spaces

    reg = take1
    value = (Val <$> num) <|> (Reg <$> reg)
    pOp op = Op op <$> (reg <* spaces) <*> value

data MState = MState
  { registersMS :: M.Map Char Int
  , commandsMS :: Zipper Command
  }

type Machine a = ExceptT (Maybe Int) (WriterT (Sum Int) (State MState)) a

currCommand :: Machine Command
currCommand = gets (viewFoci . commandsMS)

moveCommand :: Int -> Machine ()
moveCommand n = do
  z <- gets commandsMS
  case move n z of
    Nothing -> gets ((M.lookup 'h') . registersMS) >>= throwError
    Just z' -> modify (\ds -> ds { commandsMS = z' })

nextCommand :: Machine ()
nextCommand = moveCommand 1

getVal :: Value -> Machine (Maybe Int)
getVal (Val n) = pure (Just n)
getVal (Reg c) = gets (M.lookup c . registersMS)

modifyReg :: Reg -> Maybe (Int -> Int) -> Machine ()
modifyReg r Nothing = pure ()
modifyReg r (Just f) = modify (\ds -> ds {registersMS = M.adjust f r (registersMS ds)})

setReg :: Reg -> Int -> Machine ()
setReg r i = modify (\ds -> ds {registersMS = M.insert r i (registersMS ds)})

solve1 :: MState -> (Either (Maybe Int) (), Int)
solve1 = fmap getSum . evalState (runWriterT (runExceptT run))
  where
    run = forever $ currCommand >>= interpCommand

    interpCommand = \case

      Op op r v -> op & \case
        Set -> getVal v >>= setReg r . fromJust >> nextCommand
        Sub -> fmap subtract <$> getVal v >>= modifyReg r >> nextCommand
        Mul -> fmap (*) <$> getVal v >>= modifyReg r >> tell (Sum 1) >> nextCommand

      Jnz cond off -> do
        condv <- getVal cond
        offv <- getVal off
        if isJust condv && isJust offv && fromJust condv /= 0
          then moveCommand (fromJust offv)
          else nextCommand

getVal' m (Reg r) = M.findWithDefault 0 r m
getVal' m (Val v) = v

-- this boils down to:
-- how many times is h decremented until g ==0?

solve2 :: V.Vector Command -> Int
solve2 commands = go 0 (M.singleton 'a' 1)
  where
    go !i !st = case commands V.!? i of
      Nothing -> st M.! 'h'
      Just c -> c & \case
        Op op r v -> op & \case
          Set -> go (i+1) (M.insert r (getVal' st v) st)
          Sub -> go (i+1) (M.adjust (subtract (getVal' st v)) r st)
          Mul -> go (i+1) (M.adjust (* (getVal' st v)) r st)
        Jnz cond off ->
          if getVal' st cond /= 0 then
            go (i + getVal' st off) st
          else
            go (i+1) st

main :: IO ()
main = do
  Just commands <- runParser parseCommands <$> readFile "input.txt"
  print $ solve1 (MState mempty (zipperFromList commands))
