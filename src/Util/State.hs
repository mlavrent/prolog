module Util.State where
import Control.Monad.State

mapStateAndVal :: ((a, s) -> (b, t)) -> (t -> s) -> State s a -> State t b
mapStateAndVal f finv s = state (f . runState s . finv)
