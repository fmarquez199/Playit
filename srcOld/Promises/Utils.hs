module Playit.FrontEnd.Promises.Utils where


-------------------------------------------------------------------------------
checkPromises ::  S.ParserM ()
checkPromises = do
  S.ParserS{proms = promises} <- get
  fileCode <- ask

  forM_ promises $ \t ->
    case t of
      PromiseS {}         -> printErrorPromiseFunction t fileCode
      (PromiseT name pos) -> 
        tell [errorMsg ("Type '" ++ name ++ "' wasn't defined") fileCode pos]
-------------------------------------------------------------------------------


printErrorPromiseFunction :: Promise -> ParserR -> S.ParserM ()
printErrorPromiseFunction (PromiseS name args t cat pc _ _ _)  fileCode = 
  if cat == Functions then
    tell [errorMsg ("Function '" ++ name ++ "(" ++ showParamsPF args ++ ") ->" ++ show t ++ "'  wasn't defined") fileCode pc]
  else 
    tell [errorMsg ("Procedure '" ++ name ++ "(" ++ showParamsPF args ++ ")' wasn't defined") fileCode pc]
  
  where
    showParamsPF:: [(Type,Pos)] -> String
    showParamsPF []        = ""
    showParamsPF [(t,p)]   = show t
    showParamsPF ((t,p):r) = show t ++ "," ++ showParamsPF r
