module FFI.Anything.TypeUncurry.ReturnResult where

import Control.Monad.Identity


return1 :: (Monad m) => r -> m r
return1 = return

return2 :: (Monad m) => (a -> r) -> a -> m r
return2 f a = return $ f a

return3 :: (Monad m) => (a -> b -> r) -> a -> b -> m r
return3 f a b = return $ f a b

return4 :: (Monad m) => (a -> b -> c -> r) -> a -> b -> c -> m r
return4 f a b c = return $ f a b c

return5 :: (Monad m) => (a -> b -> c -> d -> r) -> a -> b -> c -> d -> m r
return5 f a b c d = return $ f a b c d

return6 :: (Monad m) => (a -> b -> c -> d -> e -> r) -> a -> b -> c -> d -> e -> m r
return6 f a b c d e = return $ f a b c d e

return7 :: (Monad m) => (a -> b -> c -> d -> e -> f -> r) -> a -> b -> c -> d -> e -> f -> m r
return7 f a b c d e = return . f a b c d e -- We don't want to use f again, haha!


returnId1 :: r -> Identity r
returnId1 = return1

returnId2 :: (a -> r) -> a -> Identity r
returnId2 = return2

returnId3 :: (a -> b -> r) -> a -> b -> Identity r
returnId3 = return3

returnId4 :: (a -> b -> c -> r) -> a -> b -> c -> Identity r
returnId4 = return4

returnId5 :: (a -> b -> c -> d -> r) -> a -> b -> c -> d -> Identity r
returnId5 = return5

returnId6 :: (a -> b -> c -> d -> e -> r) -> a -> b -> c -> d -> e -> Identity r
returnId6 = return6

returnId7 :: (a -> b -> c -> d -> e -> f -> r) -> a -> b -> c -> d -> e -> f -> Identity r
returnId7 = return7

