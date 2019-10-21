{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExplicitForAll #-}


{-# LANGUAGE TypeFamilies #-} -- weirdly needed for ~
-- {-# LANGUAGE FlexibleContexts #-}

class (Monad m) => MonadOi m where
  oi :: String -> m ()

instance MonadOi IO where
  oi s = putStrLn $ "oi, " ++ s ++ "!"

oimate :: MonadOi m => String -> m ()
oimate s = oi $ "mate, " ++ s

oimateIO :: (MonadOi m, m ~ IO) => String -> m ()
oimateIO s = oi $ "IOmate, " ++ s

ioBox :: (m ~ IO) => (forall m. MonadOi m => String -> m ()) -> m ()
ioBox f = f "input"

useIoBox :: IO ()
useIoBox = ioBox oimateIO

-- >>> useIoBox
-- oi, mate, input!

main :: IO ()
main = oimate "bruv"

-- >>> main
-- oi, mate, bruv!
