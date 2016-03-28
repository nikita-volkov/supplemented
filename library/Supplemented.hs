module Supplemented
where

import Supplemented.Prelude


{-# RULES
"essence/supplement/<*" [~2]
  forall p1 p2.
    essence p1 <* supplement p2 =
      essenceAndSupplement p1 p2
"essence/supplement/*>" [~2]
  forall p1 p2.
    essence p1 *> supplement p2 =
      essenceAndSupplement (p1 $> ()) p2
"*>/supplement" [~2]
  forall p pp.
    pp *> supplement p =
      mapSupplement (*> p) pp $> ()
"<*/supplement" [~2]
  forall p pp.
    pp <* supplement p =
      mapSupplement (*> p) pp
  #-}


newtype Supplemented m a =
  Supplemented (m (a, m ()))
  deriving (Functor)

instance Monad m => Applicative (Supplemented m) where
  {-# INLINE pure #-}
  pure a =
    Supplemented (return (a, return ()))
  {-# INLINABLE [2] (<*>) #-}
  (<*>) (Supplemented m1) (Supplemented m2) =
    Supplemented $
    do
      (result1, supplement1) <- m1
      supplement1
      (result2, supplement2) <- m2
      return (result1 result2, supplement2)

instance MonadPlus m => Alternative (Supplemented m) where
  {-# INLINE empty #-}
  empty =
    Supplemented mzero
  {-# INLINABLE [2] (<|>) #-}
  (<|>) (Supplemented m1) (Supplemented m2) =
    Supplemented $
    mplus m1 m2

instance Monad m => Monad (Supplemented m) where
  {-# INLINE return #-}
  return =
    pure
  {-# INLINABLE (>>=) #-}
  (>>=) (Supplemented m1) k2 =
    Supplemented $
    do
      (result1, supplement1) <- m1
      supplement1
      case k2 result1 of
        Supplemented m2 ->
          m2

instance MonadPlus m => MonadPlus (Supplemented m) where
  {-# INLINE mzero #-}
  mzero =
    empty
  {-# INLINE mplus #-}
  mplus =
    (<|>)

instance MonadTrans Supplemented where
  {-# INLINE lift #-}
  lift =
    essence

{-# INLINE [2] essence #-}
essence :: Monad m => m a -> Supplemented m a
essence essence =
  Supplemented (fmap (\r -> (r, return ())) essence)

{-# INLINE [2] supplement #-}
supplement :: Monad m => m () -> Supplemented m ()
supplement supplement =
  Supplemented (return ((), supplement))

{-# INLINE [2] essenceAndSupplement #-}
essenceAndSupplement :: Monad m => m a -> m () -> Supplemented m a
essenceAndSupplement essence supplement =
  Supplemented (fmap (\r -> (r, supplement)) essence)

{-# INLINE [2] mapSupplement #-}
mapSupplement :: Monad m => (m () -> m ()) -> Supplemented m a -> Supplemented m a
mapSupplement mapping (Supplemented m) =
  Supplemented ((fmap . fmap) mapping m)
