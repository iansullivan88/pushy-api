module Pushy.Web.CommonRoutes
    (
      Routes
    , register, route, withHandler
    ) where

import Control.Monad(ap, liftM)
import Control.Monad.State.Strict(State, execState, modify)
import Data.Foldable(traverse_)

newtype Routes r h a = Routes { routes :: State [Route r h] a }

instance Functor (Routes r h) where
    fmap = liftM

instance Applicative (Routes r h) where
    pure  = return
    (<*>) = ap

instance Monad (Routes r h) where
    return         = Routes . return
    Routes s >>= f = Routes $ s >>= routes . f


data Route r h = Route r h

route :: r -> h -> Routes r h ()
route r h = Routes $ modify (Route r h :)

register :: (Monad m) => (r -> h -> m ()) -> Routes r h () -> m ()
register reg (Routes st) = 
    let routes = execState st []
    in  traverse_ registerRoute (reverse routes) where
    registerRoute (Route r h) = reg r h

withHandler :: (h' -> h) -> Routes r h' () -> Routes r h ()
withHandler f (Routes st') =
    let routes' = execState st' []
        routes = fmap (\(Route r h') -> Route r (f h')) routes'
    in  Routes $ modify (routes ++)

