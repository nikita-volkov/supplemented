module Supplemented
where

import Supplemented.Prelude


{-# RULES
"essence/supplement/<*" [~2]
  forall p1 p2.
    essence p1 <* supplement p2 =
      Supplemented p1 p2
"essence/supplement/*>" [~2]
  forall p1 p2.
    essence p1 *> supplement p2 =
      Supplemented (p1 $> ()) p2
"essence/*>" [~2]
  forall parser main. 
    essence parser *> main =
      mapEssence (parser *>) main
"essence/<*" [~2]
  forall parser main. 
    essence parser <* main =
      mapEssence (parser <*) main
"mapSupplement/mapEssence" [~2]
  forall essenceFn supplementFn partialParser. 
    mapSupplement supplementFn (mapEssence essenceFn partialParser) =
      mapEssenceAndSupplement essenceFn supplementFn partialParser
"mapEssence/mapSupplement" [~2]
  forall essenceFn supplementFn partialParser. 
    mapEssence essenceFn (mapSupplement supplementFn partialParser) =
      mapEssenceAndSupplement essenceFn supplementFn partialParser
"*>/supplement" [~2]
  forall p pp.
    pp *> supplement p =
      mapSupplement (*> p) pp
"<*/supplement" [~2]
  forall p pp.
    pp <* supplement p =
      mapEssenceAndSupplement ($> ()) (*> p) pp
  #-}


data Supplemented m a =
  -- |
  -- The essence and supplement.
  Supplemented !(m a) (m ())
  deriving (Functor)

instance Applicative m => Applicative (Supplemented m) where
  {-# INLINE pure #-}
  pure a =
    Supplemented (pure a) (pure ())
  {-# INLINABLE [2] (<*>) #-}
  (<*>) (Supplemented essence1 supplement1) (Supplemented essence2 supplement2) =
    Supplemented (essence1 <* supplement1 <*> essence2) supplement2

instance Alternative m => Alternative (Supplemented m) where
  {-# INLINE empty #-}
  empty =
    Supplemented empty (pure ())
  {-# INLINABLE [2] (<|>) #-}
  (<|>) (Supplemented essence1 supplement1) (Supplemented essence2 supplement2) =
    Supplemented (essence1 <* supplement1 <|> essence2) supplement2

{-# INLINE [2] essence #-}
essence :: Applicative m => m a -> Supplemented m a
essence essence =
  Supplemented essence (pure ())

{-# INLINE [2] supplement #-}
supplement :: Applicative m => m () -> Supplemented m ()
supplement supplement =
  Supplemented (pure ()) supplement

{-# INLINE [2] mapEssence #-}
mapEssence :: (m a -> m b) -> (Supplemented m a -> Supplemented m b)
mapEssence fn (Supplemented essence supplement) =
  Supplemented (fn essence) supplement

{-# INLINE [2] mapSupplement #-}
mapSupplement :: (m () -> m ()) -> (Supplemented m a -> Supplemented m a)
mapSupplement fn (Supplemented essence supplement) =
  Supplemented essence (fn supplement)

{-# INLINABLE [2] mapEssenceAndSupplement #-}
mapEssenceAndSupplement :: (m a -> m b) -> (m () -> m ()) -> (Supplemented m a -> Supplemented m b)
mapEssenceAndSupplement essenceFn supplementFn (Supplemented essence supplement) =
  Supplemented (essenceFn essence) (supplementFn supplement)
