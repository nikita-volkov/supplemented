module Supplemented
(
  Supplemented,
  runSupplemented,
  essence,
  supplement,
  essenceAndSupplement,
  mapSupplement,
)
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
  Supplemented (Either (a, m ()) (m (a, m ())))

instance Functor m => Functor (Supplemented m) where
  {-# INLINABLE fmap #-}
  fmap f (Supplemented either1) =
    Supplemented either2
    where
      either2 =
        case either1 of
          Left (result1, supplement1) ->
            Left (f result1, supplement1)
          Right m1 ->
            Right $
            fmap (\(result1, supplement1) -> (f result1, supplement1)) $
            m1

instance Monad m => Applicative (Supplemented m) where
  {-# INLINE pure #-}
  pure a =
    Supplemented (Left (a, pure ()))
  {-# INLINABLE [2] (<*>) #-}
  (<*>) (Supplemented either1) (Supplemented either2) =
    Supplemented either3
    where
      either3 =
        case either1 of
          Left (result1, supplement1) ->
            case either2 of
              Left (result2, supplement2) ->
                Left (result1 result2, supplement1 *> supplement2)
              Right m2 ->
                Right $
                fmap (\(result2, supplement2) -> (result1 result2, supplement2)) $
                supplement1 *> m2
          Right m1 ->
            case either2 of
              Left (result2, supplement2) ->
                Right $
                fmap (\(result1, supplement1) -> (result1 result2, supplement1 *> supplement2)) $
                m1
              Right m2 ->
                Right $
                do
                  (result1, supplement1) <- m1
                  supplement1
                  (result2, supplement2) <- m2
                  return (result1 result2, supplement2)

instance MonadPlus m => Alternative (Supplemented m) where
  {-# INLINE empty #-}
  empty =
    Supplemented (Right mzero)
  {-# INLINABLE [2] (<|>) #-}
  (<|>) (Supplemented either1) (Supplemented either2) =
    Supplemented either3
    where
      either3 =
        Right (mplus (m either1) (m either2))
        where
          m =
            either (\(result, supplement) -> (result, return ()) <$ supplement) id

instance Monad m => Monad (Supplemented m) where
  {-# INLINE return #-}
  return =
    pure
  {-# INLINABLE (>>=) #-}
  (>>=) (Supplemented either1) k2 =
    Supplemented either3
    where
      either3 =
        case either1 of
          Left (result1, supplement1) ->
            case k2 result1 of
              Supplemented either2 ->
                case either2 of
                  Left (result2, supplement2) ->
                    Left (result2, supplement1 *> supplement2)
                  Right m2 ->
                    Right (supplement1 *> m2)
          Right m1 ->
            Right $
            do
              (result1, supplement1) <- m1
              case k2 result1 of
                Supplemented either2 ->
                  case either2 of
                    Left (result2, supplement2) ->
                      return (result2, supplement1 *> supplement2)
                    Right m2 ->
                      do
                        supplement1
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


{-# INLINE runSupplemented #-}
runSupplemented :: Monad m => Supplemented m a -> m (a, m ())
runSupplemented (Supplemented either1) =
  either return id either1

{-# INLINE [2] essence #-}
essence :: Monad m => m a -> Supplemented m a
essence essence =
  Supplemented (Right (fmap (\r -> (r, return ())) essence))

{-# INLINE [2] supplement #-}
supplement :: Monad m => m () -> Supplemented m ()
supplement supplement =
  Supplemented (Left ((), supplement))

{-# INLINE [2] essenceAndSupplement #-}
essenceAndSupplement :: Monad m => m a -> m () -> Supplemented m a
essenceAndSupplement essence supplement =
  Supplemented (Right (fmap (\r -> (r, supplement)) essence))

{-# INLINE [2] mapSupplement #-}
mapSupplement :: Monad m => (m () -> m ()) -> Supplemented m a -> Supplemented m a
mapSupplement mapping (Supplemented either1) =
  Supplemented $
  case either1 of
    Left (result1, supplement1) ->
      Left (result1, mapping supplement1)
    Right m1 ->
      Right $
      fmap (\(result1, supplement1) -> (result1, mapping supplement1)) $
      m1
