import qualified Data.Map as M

let a = M.fromList [(1,[Just 2,Nothing]),(2,[Just 4,Nothing])]

let isJust2 Just a = a == 2
let isJust2 Nothing = False

let change

M.adjust (map (\x -> if isJust2 x then fmap (+1) else x)  ) 1 a

