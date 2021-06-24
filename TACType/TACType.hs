module TACType where


class SymEntryCompatible a where
  getSymID :: a -> String


data (SymEntryCompatible a) => ThreeAddressCode a b = ThreeAddressCode
  { tacOperand :: Operation
  , tacLvalue  :: Maybe (Operand a b)
  , tacRvalue1 :: Maybe (Operand a b)
  , tacRvalue2 :: Maybe (Operand a b)
  } deriving (Eq, Ord)


instance (SymEntryCompatible a, Show a, Show b) => Show (ThreeAddressCode a b) where
  show (ThreeAddressCode Assign (Just x) (Just y) Nothing)  = "\t" ++ show x ++ " := " ++ show y
  -- For null pointer?
  show (ThreeAddressCode Assign (Just x) Nothing Nothing)   = "\t" ++ show x ++ " := 0"
  -- Arithmetic
  show (ThreeAddressCode Add (Just x) (Just y) (Just z))    = "\t" ++ show x ++ " := " ++ show y ++ " + " ++ show z
  show (ThreeAddressCode Sub (Just x) (Just y) (Just z))    = "\t" ++ show x ++ " := " ++ show y ++ " - " ++ show z
  show (ThreeAddressCode Minus (Just x) (Just y) Nothing)   = "\t" ++ show x ++ " := -" ++ show y 
  show (ThreeAddressCode Mult (Just x) (Just y) (Just z))   = "\t" ++ show x ++ " := " ++ show y ++ " * " ++ show z
  show (ThreeAddressCode Div (Just x) (Just y) (Just z))    = "\t" ++ show x ++ " := " ++ show y ++ " / " ++ show z
  show (ThreeAddressCode Mod (Just x) (Just y) (Just z))    = "\t" ++ show x ++ " := " ++ show y ++ " % " ++ show z
  -- Logical
  show (ThreeAddressCode And (Just x) (Just y) (Just z))    = show x ++ " := " ++ show y ++ " && " ++ show z
  show (ThreeAddressCode Or (Just x) (Just y) (Just z))     = show x ++ " := " ++ show y ++ " || " ++ show z
  -- Comparators
  show (ThreeAddressCode Gt (Just x) (Just y) (Just z))     = "\t" ++ "if " ++ show x ++ " > " ++ show y ++ " goto " ++ show z
  show (ThreeAddressCode Gte (Just x) (Just y) (Just z))    = "\t" ++ "if " ++ show x ++ " >= " ++ show y ++ " goto " ++ show z
  show (ThreeAddressCode Lt (Just x) (Just y) (Just z))     = "\t" ++ "if " ++ show x ++ " < " ++ show y ++ " goto " ++ show z
  show (ThreeAddressCode Lte (Just x) (Just y) (Just z))    = "\t" ++ "if " ++ show x ++ " <= " ++ show y ++ " goto " ++ show z
  show (ThreeAddressCode Eq (Just x) (Just y) (Just z))     = "\t" ++ "if " ++ show x ++ " == " ++ show y ++ " goto " ++ show z
  show (ThreeAddressCode Neq (Just x) (Just y) (Just z))    = "\t" ++ "if " ++ show x ++ " != " ++ show y ++ " goto " ++ show z
  -- Jumping
  show (ThreeAddressCode GoTo Nothing Nothing (Just label)) = "\t" ++ "goto " ++ show label
  show (ThreeAddressCode GoTo Nothing Nothing Nothing)      = "\t" ++ "goto __"
  show (ThreeAddressCode If Nothing (Just b) (Just label))  = "\t" ++ "if " ++ show b ++ " goto " ++ show label
  show (ThreeAddressCode If (Just b) Nothing Nothing)       = "\t" ++ "if " ++ show b ++ " goto __"
  show (ThreeAddressCode IfFalse Nothing (Just b) (Just l)) = "\t" ++ "if !" ++ show b ++ " goto " ++ show l
  show (ThreeAddressCode NewLabel (Just l) Nothing Nothing) = show l ++ ": "
  -- Calling functions
  show (ThreeAddressCode Param Nothing (Just p) _)          = "\tparam " ++ show p
  show (ThreeAddressCode Param (Just arg) _ _)              = "\targ " ++ show arg
  show (ThreeAddressCode Call Nothing (Just f) (Just n))    = "\tcall " ++ show f ++ ", " ++ show n
  show (ThreeAddressCode Call (Just x) (Just f) (Just n))   = "\t" ++ show x ++ " := call " ++ show f ++ ", " ++ show n
  show (ThreeAddressCode Return Nothing Nothing Nothing)    = "\treturn" 
  show (ThreeAddressCode Return Nothing (Just x) Nothing)   = "\treturn " ++ show x 
  -- Array operators
  show (ThreeAddressCode Get (Just x) (Just y) (Just i))    = "\t" ++ show x ++ " := " ++ show y ++ "[" ++ show i ++ "]"
  show (ThreeAddressCode Set (Just x) (Just i) (Just y))    = "\t" ++ show x ++ "[" ++ show i ++ "] := " ++ show y
  show (ThreeAddressCode Set (Just x) (Just i) Nothing)     = "\t" ++ show x ++ "[" ++ show i ++ "] "
  show (ThreeAddressCode Anexo (Just x) (Just y) (Just z))  = "\t" ++ show x ++ " := " ++ show y ++ " : " ++ show z
  show (ThreeAddressCode Concat (Just x) (Just y) (Just z)) = "\t" ++ show x ++ " := " ++ show y ++ " ++ " ++ show z
  show (ThreeAddressCode Len (Just x) (Just y) _)        = "\t" ++ show x ++ " := #" ++ show y
  -- Pointer operations
  show (ThreeAddressCode Ref (Just x) (Just y) Nothing)     = "\t" ++ show x ++ " := &" ++ show y
  show (ThreeAddressCode Deref (Just x) (Just y) Nothing)   = "\t" ++ show x ++ " := *" ++ show y
  show (ThreeAddressCode Malloc (Just var) (Just size) Nothing) = "\t" ++ show var ++ " := malloc(" ++ show size ++ ")"
  -- Access for records and unions
  show (ThreeAddressCode Access (Just x) (Just r) (Just f)) = "\t" ++ show x ++ " := " ++ show r ++ "." ++ show f
  -- Input/Output
  show (ThreeAddressCode Read Nothing (Just e) _)       = "\tread " ++ show e
  show (ThreeAddressCode Read Nothing Nothing _)        = "\tread"
  show (ThreeAddressCode Print Nothing (Just e) _)      = "\tprint " ++ show e
  -- Exit program
  show (ThreeAddressCode Exit Nothing Nothing Nothing)   = "\texit"
  show (ThreeAddressCode Abort Nothing Nothing Nothing)  = "\tabort"
  -- Castings
  show (ThreeAddressCode (Cast _ toT) (Just x) (Just y) _)  = show x ++ " := " ++ toT ++ "(" ++ show y ++ ")"
  -- Operator no recognized
  show tac = show (tacLvalue tac) ++ " := " ++ show (tacRvalue1 tac) ++ " " ++ show (tacOperand tac) ++ " " ++ show (tacRvalue2 tac)


data (SymEntryCompatible a) => Operand a b 
  = Id a
  | Constant (String, b)
  | Label String
  deriving (Eq, Ord)


instance (SymEntryCompatible a, Show a, Show b) => Show (Operand a b) where
  show (Id x)       = show x
  show (Constant c) = fst c
  show (Label l)    = l


data Operation =
  Assign            |
-- Arithmetic
  -- | Addition (add)
  Add               |
  -- | Substraction (sub)
  Sub               |
  -- | Unary minus (neg)
  Minus             |
  -- | Multiplication (mul)
  Mult              |
  -- | Div (div)
  Div               |
  -- | Modulus (rem)
  Mod               |

-- Logical
  -- | Logical and
  And               |
  -- | Logical or
  Or                |

-- Comparators
  -- | Gt than (bgt)
  Gt                |
  -- | Gt than or equal (bge)
  Gte               |
  -- | Lt than (blt)
  Lt                |
  -- | Lt than or equal (bte)
  Lte               |
  -- | Equal (beq)
  Eq                |
  -- | Not equal (bne)
  Neq               |

-- Jumping
  -- | goto <label> "b"
  GoTo              |
  -- | if <var> goto <label> "bnez"
  If                |
  -- | if !<var> goto <label>
  IfFalse           |
  -- | <label> :
  NewLabel          |

-- Calling functions
  -- | Define a parameter
  Param             |
  -- | Call function (jal)
  Call              |
  -- | Return value from function (jr)
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
  Len            |

-- Pointer operations
  -- | x:= &y (la)
  Ref               |
  -- | x:= *y (lw)
  Deref             |
  -- | make syscall 9 to allocate size of memory
  Malloc            |

-- Access for records and unions
  -- | x:= r.field
  Access            |

-- Input/Output
  -- | read x
  Read        |
  -- | print x
  Print       |

-- Exit program
  -- | exit (successful)
  Exit        |
  -- | abort (failure)
  Abort       |

-- Castings
  Cast String String

  deriving (Eq, Show, Ord)
