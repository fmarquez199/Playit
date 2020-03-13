module Playit.BackEnd.TACType where


class SymEntryCompatible a where
  getSymID :: a -> String


data (SymEntryCompatible a) => ThreeAddressCode a b = TACC
  { tacOperand :: Operation,
    tacLvalue  :: Maybe (Operand a b),
    tacRvalue1 :: Maybe (Operand a b),
    tacRvalue2 :: Maybe (Operand a b)
  }
  deriving (Eq)


instance (SymEntryCompatible a, Show a, Show b) => Show (ThreeAddressCode a b) where
  show (TACC Assign (Just x) (Just y) Nothing)  = "\t" ++ show x ++ " := " ++ show y
-- For null pointer?
  show (TACC Assign (Just x) Nothing Nothing)   = "\t" ++ show x ++ " := 0"
-- Arithmetic
  show (TACC Add (Just x) (Just y) (Just z))    = "\t" ++ show x ++ " := " ++ show y ++ " + " ++ show z
  show (TACC Sub (Just x) (Just y) (Just z))    = "\t" ++ show x ++ " := " ++ show y ++ " - " ++ show z
  show (TACC Minus (Just x) (Just y) Nothing)   = "\t" ++ show x ++ " := -" ++ show y 
  show (TACC Mult (Just x) (Just y) (Just z))   = "\t" ++ show x ++ " := " ++ show y ++ " * " ++ show z
  show (TACC Div (Just x) (Just y) (Just z))    = "\t" ++ show x ++ " := " ++ show y ++ " / " ++ show z
  show (TACC Mod (Just x) (Just y) (Just z))    = "\t" ++ show x ++ " := " ++ show y ++ " % " ++ show z
-- Logical
  show (TACC And (Just x) (Just y) (Just z))    = show x ++ " := " ++ show y ++ " && " ++ show z
  show (TACC Or (Just x) (Just y) (Just z))     = show x ++ " := " ++ show y ++ " || " ++ show z
  -- show (TACC Not (Just x) (Just y) _)           = "\t" ++ show x ++ " := !" ++ show y
-- Comparators
  show (TACC Gt (Just x) (Just y) (Just z))     = "\t" ++ "if " ++ show x ++ " > " ++ show y ++ " goto " ++ show z
  show (TACC Gte (Just x) (Just y) (Just z))    = "\t" ++ "if " ++ show x ++ " >= " ++ show y ++ " goto " ++ show z
  show (TACC Lt (Just x) (Just y) (Just z))     = "\t" ++ "if " ++ show x ++ " < " ++ show y ++ " goto " ++ show z
  show (TACC Lte (Just x) (Just y) (Just z))    = "\t" ++ "if " ++ show x ++ " <= " ++ show y ++ " goto " ++ show z
  show (TACC Eq (Just x) (Just y) (Just z))     = "\t" ++ "if " ++ show x ++ " == " ++ show y ++ " goto " ++ show z
  show (TACC Neq (Just x) (Just y) (Just z))    = "\t" ++ "if " ++ show x ++ " != " ++ show y ++ " goto " ++ show z
-- Jumping
  show (TACC GoTo Nothing Nothing (Just label)) = "\t" ++ "goto " ++ show label
  show (TACC GoTo Nothing Nothing Nothing)      = "\t" ++ "goto __"
  show (TACC If Nothing (Just b) (Just label))  = "\t" ++ "if " ++ show b ++ " goto " ++ show label
  show (TACC If (Just b) Nothing Nothing)       = "\t" ++ "if " ++ show b ++ " goto __"
  show (TACC IfFalse Nothing (Just b) (Just l)) = "\t" ++ "if !" ++ show b ++ " goto " ++ show l
  show (TACC NewLabel (Just l) Nothing Nothing) = show l ++ ": "
-- Calling functions
  show (TACC Param Nothing (Just p) Nothing)    = "\tparam " ++ show p
  show (TACC Call Nothing (Just f) (Just n))    = "\tcall " ++ show f ++ ", " ++ show n
  show (TACC Call (Just x) (Just f) (Just n))   = "\t" ++ show x ++ " := call " ++ show f ++ ", " ++ show n
  show (TACC Return Nothing Nothing Nothing)    = "\treturn" 
  show (TACC Return Nothing (Just x) Nothing)   = "\treturn " ++ show x 
-- Array operators
  show (TACC Get (Just x) (Just y) (Just i))    = "\t" ++ show x ++ " := " ++ show y ++ "[" ++ show i ++ "]"
  show (TACC Set (Just x) (Just i) (Just y))    = "\t" ++ show x ++ "[" ++ show i ++ "] := " ++ show y
  show (TACC Anexo (Just x) (Just y) (Just z))  = "\t" ++ show x ++ " := " ++ show y ++ " : " ++ show z
  show (TACC Concat (Just x) (Just y) (Just z)) = "\t" ++ show x ++ " := " ++ show y ++ " ++ " ++ show z
  show (TACC Length (Just x) (Just y) _)        = "\t" ++ show x ++ " := #" ++ show y
-- Pointer operations
  show (TACC Ref (Just x) (Just y) Nothing)     = "\t" ++ show x ++ " := &" ++ show y
  show (TACC Deref (Just x) (Just y) Nothing)   = "\t" ++ show x ++ " := *" ++ show y
  -- show (TACC New (Just x) (Just size) Nothing)  = "\t" ++ show x ++ " := malloc(" ++ show size ++ ")"
  -- show (TACC Free Nothing (Just addr) Nothing)  = "\tfree(" ++ show addr ++ ")"
  -- show (TACC Free Nothing Nothing Nothing)      = "\tSegmentation fault"
-- Access for records and unions
  show (TACC Access (Just x) (Just r) (Just f)) = "\t" ++ show x ++ " := " ++ show r ++ "." ++ show f
-- Castings
  show (TACC (Cast _ toT) (Just x) (Just y) _)  = show x ++ " := " ++ toT ++ "(" ++ show y ++ ")"
-- Operator no recognized
  show tac = show (tacLvalue tac) ++ " := " ++ show (tacRvalue1 tac) ++ " " ++ show (tacOperand tac) ++ " " ++ show (tacRvalue2 tac)


data (SymEntryCompatible a) => Operand a b = 
  Variable a           | 
  Constant (String, b) | 
  Label String
  deriving (Eq, Ord)


instance (SymEntryCompatible a, Show a, Show b) => Show (Operand a b) where
  show (Variable x) = show x
  show (Constant c) = fst c
  show (Label l)    = l


data Operation =
  Assign            |
-- Arithmetic
  -- | Addition
  Add               |
  -- | Substraction
  Sub               |
  -- | Unary minus
  Minus             |
  -- | Multiplication
  Mult              |
  -- | Division
  Div               |
  -- | Modulus
  Mod               |

-- Logical
  -- | Logical and
  And               |
  -- | Logical or
  Or                |
  -- | Logical not, >>>> no es necesario porque se hace jumping code <<<<
  -- Not               |

-- Comparators
  -- | Greater than
  Gt                |
  -- | Greater than or equal
  Gte               |
  -- | Less than
  Lt                |
  -- | Less than or equal
  Lte               |
  -- | Equal
  Eq                |
  -- | Not equal
  Neq               |

-- Jumping
  -- | goto <label>
  GoTo              |
  -- | if <var> goto <label>
  If                |
  -- | if !<var> goto <label>
  IfFalse           |
  -- | <label> :
  NewLabel          |

-- Calling functions
  -- | Define a parameter
  Param             |
  -- | Call function
  Call              |
  -- | Return value from function
  Return            |

-- Array operators
  -- | x:= y[i]
  Get               |
  -- | x[i]:= y
  Set               |
  -- | x:= 5:y
  Anexo             |
  -- | x:= y++z
  Concat            |
  -- | x:= #y
  Length            |

-- Pointer operations
  -- | x:= &y
  Ref               |
  -- | x:= *y
  Deref             |
  -- | x:= new Type, >>>> No es necesario porque se llama a malloc <<<
  -- New               |
  -- | free var, >>>> No es necesario porque se llama a free <<<
  -- Free              |

-- Access for records and unions
  -- | x:= r.field
  Access            |

-- Castings
  Cast String String

  deriving (Eq, Show)
