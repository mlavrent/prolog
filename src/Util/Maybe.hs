module Util.Maybe where

import Data.Maybe

mcons :: Maybe a -> [a] -> [a]
mcons ma as = maybeToList ma ++ as
