import Control.Monad.Memo


myAdd  :: (MonadMemo Int Int m) => Int -> Int -> m Int
myAdd a 0 = return a
myAdd a b = (1 +) <$> memo (myAdd a) (b - 1)

myMult :: (MonadMemo Int Int m) => Int -> Int -> m Int
myMult a 1 = return a
myMult a b = (a +) <$> memo (myMult a) (b - 1)

myOp :: (MonadMemo Int Int m) => Int -> m Int
myOp n = do
    a <- memo (myAdd 5) n
    b <- memo (myMult 5) n
    return (a + b)
