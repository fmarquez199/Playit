{-
* Manejo de errores
*
* Copyright : (c) 
*  Manuel Gonzalez     11-10390
*  Francisco Javier    12-11163
*  Natascha Gamboa     12-11250
-}


module Playit.ErrorM  where
import Control.Monad

data ErrorM a = MonadSymTab a | Failed String

instance Monad ErrorM where
  return      = MonadSymTab 
  fail        = Failed
  MonadSymTab a  >>= f = f a
  Failed s >>= _ = Failed s

instance Applicative ErrorM where
  pure = MonadSymTab
  (Failed s) <*> _ = Failed s
  (MonadSymTab f) <*> o  = liftM f o


instance Functor ErrorM where
  fmap = liftM


thenE :: ErrorM a -> (a -> ErrorM b) -> ErrorM b
m `thenE` k =
    case m of
        MonadSymTab a -> k a
        Failed e -> Failed e


returnE :: a -> ErrorM a
returnE =  MonadSymTab

failE :: String -> ErrorM a
failE err = Failed err

catchE :: ErrorM a -> (String -> ErrorM a) -> ErrorM a
catchE m k =
    case m of
        MonadSymTab a -> MonadSymTab a
        Failed e -> k e

