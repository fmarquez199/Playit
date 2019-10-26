

data Car c = Chevrolet c deriving (Show);


instance Functor Car where  
    fmap f (Chevrolet c) = Chevrolet (f c)
    
    
