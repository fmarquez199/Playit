module Playit.FrontEnd.Promises.Types where

-- import Playit.FrontEnd.Types


-- type Promises = [Promise]

-- -- Subroutine promise for co-recursive subroutines and Registers/unions
-- data Promise = PromiseS {
--   promiseId           :: Id,
--   promiseParams       :: [(Type,Pos)],
--   promiseType         :: Type,
--   promiseCat          :: Category,
--   promisePos          :: Pos,
--   promiseLateCheck    :: [LateCheckPromise],
--   -- Llamadas a funciones que se deben chequear cuando se actualiza el tipo de 
--   -- retorno de esta promesa
--   -- esta promesa aparece en las expresiones de llamadas a funciones
--   otherCallsLateCheck :: [LateCheckPromise],
--   forEachLateCheck    :: [LateCheckPromise]
--   } | 
--   PromiseT {
--     promiseId  :: Id,
--     promisePos :: Pos
--   }
--   deriving (Eq, Ord,Show)

-- -- 
-- -- Power a = a() > b()? 1:2
-- -- Power a = #(a() :: b())==10 ? 1:2
-- -- 
-- data LateCheckPromise = 
--   LateCheckPromS {
--     expr        :: Expr,   -- Expresion que debe ser evaluada cuando se actualiza el tipo de la promesa
--     argsPos     :: [Pos],  -- Posiciones (linea,columna) de los argumentos necesarios para el check
--     linkedProms :: [Id]    -- Otras promesas enlazadas a este check (su relacionado)
--   } | 
--   LateCheckPromCall {
--     promCall    :: Subroutine,  -- Llamada que se debe evaluar
--     linkedProms :: [Id]         -- Promesas enlazadas
--   } | 
--   LateCheckPromForE {
--     expr        :: Expr, -- Llamada que se debe evaluar
--     varId       :: Id,   -- Promesas enlazadas
--     varType     :: Type, -- Promesas enlazadas
--     exprPos     :: Pos,
--     linkedProms :: [Id]  -- Otras promesas enlazadas a este check (su relacionado)
--   }
--   deriving (Eq, Ord,Show)

-- data PromiseExtraI = PromiseExtraI{}