module Playit.ParserSpec (spec) where

import Test.Hspec
import Control.Monad.Trans.RWS
import Playit.Parser
import Playit.SymbolTable(initState)
import Playit.Lexer(alexScanTokens, Token)
import Playit.Types

spec :: Spec
spec = do
  describe "Parser (parse)" $ do
    it "Something" $ do
      pending
    -- it "Throws an Exception when the file is empty" $ do
    --   run [] "FileEmpty.game" `shouldThrow` anyException

    -- it "Test Case: EmptyProg.game" $ do
    --   (ast, _, _) <- run emptyProg' "EmptyProg.game"
    --   show ast `shouldBe` "\nworld:\n\n"

    -- it "Test Case: Comments.game" $ do
    --   (ast, _, _) <- run comments' "Comments.game"
    --   show ast `shouldBe` "\nworld:\n\n"

    -- it "Test Case: Apuntadores.game" $ do
    --   (ast, _, _) <- run apuntadores' "Apuntadores.game"
    --   show ast `shouldBe` astApuntadores

    -- it "Test Case: ArrayListas.game" $ do
    --   (ast, _, _) <- run arrayListas' "ArrayListas.game" 
    --   show ast `shouldBe` astArrayListas

    -- it "Test Case: CasoMalicia01.game" $ do
    --   (ast, _, _) <- run malicia1' "CasoMalicia01.game" 
    --   show ast `shouldBe` astMalicia1

    -- it "Test Case: CasoRicardo.game" $ do
    --   (ast, _, _) <- run ricardo' "CasoRicardo.game" 
    --   show ast `shouldBe` astRicardo

    -- it "Test Case: Factorial.game" $ do
    --   (ast, _, _) <- run factorial' "Factorial.game"
    --   show ast `shouldBe` astFactorial

    -- it "Test Case: FactorialIterativo.game" $ do
    --   (ast, _, _) <- run factorialIterativo' "FactorialIterativo.game"
    --   show ast `shouldBe` astFactorialIterativo

    -- it "Test Case: Fibonnaci.game" $ do
    --   (ast, _, _) <- run fibonnaci' "Fibonnaci.game"
    --   show ast `shouldBe` astFibonnaci

    -- it "Test Case: FragmentosDeCodigo.game" $ do
    --   (ast, _, _) <- run fragmentos' "FragmentosDeCodigo.game"
    --   show ast `shouldBe` astFragmentos

    -- it "Test Case: HolaMundo.game" $ do
    --   (ast, _, _) <- run hola' "HolaMundo.game"
    --   show ast `shouldBe` astHola

    -- it "Test Case: ParOImpar.game" $ do
    --   (ast, _, _) <- run parOImpar' "ParOImpar.game"
    --   show ast `shouldBe` astParOImpar

    -- it "Test Case: PrintTreeInOrder.game" $ do
    --   (ast, _, _) <- run treeInOrder' "PrintTreeInOrder.game"
    --   show ast `shouldBe` astTreeInOrder

    -- it "Test Case: DeclaracionesYAsignaciones.game" $ do
    --   (ast, _, _) <- run declAsng' "DeclaracionesYAsignaciones.game"
    --   show ast `shouldBe` astDeclAsng

    --  it "Test Case: Registros.game" $ do
    --   (ast, _, _) <- run registros' "Registros.game"
    --   show ast `shouldBe` astRegistros

    --  it "Test Case: Subrutinas.game" $ do
    --   (ast, _, _) <- run subrutinas' "Subrutinas.game"
    --   show ast `shouldBe` astSubrutinas

    -- it "Test Case: Tablas.game" $ do pending
    --   -- (ast, _, _) <- run tablas' "Tablas.game"
    --   -- show ast `shouldBe` astTablas

    -- it "Test Case: VolumenCubo.game" $ do
    --   (ast, _, _) <- run volumen' "VolumenCubo.game"
    --   show ast `shouldBe` astVolumen

    -- it "Test Case: WrongIds.game" $ do
    --   run wrong' "WrongIds.game" `shouldThrow` anyException

    -- it "Test Case: WrongProgName.game" $ do
    --   run wrongProg' "WrongProgName.game" `shouldThrow` anyException

    -- it "Test Case: EmptyProgName.game" $ do 
    --   run emptyProgName' "EmptyProgName.game" `shouldThrow` anyException

    -- it "Test Case: CasoManuel.game" $ do 
    --   pending

emptyProg :: String
emptyProg = "world %ProgramaVacio%:\n.~"

emptyProg' :: [Token]
emptyProg' = alexScanTokens emptyProg

comments :: String
comments = "world %comments%:\n  \"'Several-lines-Comment'\"\n  @One-Line-Comment\n.~"

comments' :: [Token]
comments' = alexScanTokens comments

-- run :: [Token] -> String -> (Instr, (SymTab, ActiveScopes, Alcance), [String])
-- run f n = runRWST (parse f) n initState

apuntadores :: String
apuntadores = "Inventory Nombre:\n  Skill flotante\n.~\n\nInventory Contacto:\n  Nombre puff pointernombre\n.~\n\nInventory Contacto2:\n  Nombre pointernombre2\n.~\n\nworld %apts%:\n  Contacto puff puff c = summon Contacto @, c2, c3\n\n  @ ( *( (**c).pointernombre)).flotante\n  puff (puff (puff c) spawn pointernombre) spawn flotante = 2'2\n\n  @ puff c spawn pointernombre2 spawn flotante = 2'2 @ reg no tiene campo\n  @ puff c spawn p spawn flotante = 2'2 @ campo no declarado\n\n\n  Power puff apt\n  Kit of Power puff apt2\n  Kit of Power puff puff apt3\n  Kit of Power puff puff puff apt4\n\n  c = puff apt == puff apt2|>12<|\n  c = puff apt == puff puff apt3|>12<|\n  c = puff apt == puff puff puff apt4|>12<|\n\n.~"

apuntadores' :: [Token]
apuntadores' = alexScanTokens apuntadores

astApuntadores = "\nworld:\n  c = summon Contacto\n  puff (puff (puff (c)) spawn pointernombre) spawn flotante = 2.2\n  c = (puff (apt) == puff (apt2) index: 12)\n  c = (puff (apt) == puff (puff (apt3)) index: 12)\n  c = (puff (apt) == puff (puff (puff (apt4))) index: 12)\n"

arrayListas :: String
arrayListas = "Inventory Contacto:\n  Runes id\n.~\n\nworld %test%:\n  Kit of Contacto lista\n  Contacto c1, c2, c3\n  Contacto|}10{| array = |)c1,c2,c3(|\n\n  Power|}5{| puff a0, puff a1\n\n  Power a2, a3\n\n  Kit of Kit of Kit of Power asd\n.~\n"

arrayListas' :: [Token]
arrayListas' = alexScanTokens arrayListas

astArrayListas :: String
astArrayListas = "\nworld:  array = [c3,c2,c1]\n"

malicia1 :: String
malicia1 = "\"'******************************************************************************\n*\n*                           Este caso de prueba deberia compilar!\n*\n*                           Tester: Manuel Gonzalez\n*                           Lenguaje: Playit\n* \n* \n*****************************************************************************'\"\n\nInventory PrimerNombre2:\n  Skill flotante\n  Power entero\n.~\n\nInventory Nombre2:\n  PrimerNombre2 puff regpn\n.~\n\nInventory Contacto2:\n  Nombre2 regnombre\n  Skill flotante\n.~\n\nInventory Contacto3:\n  Nombre2 puff pointernombre\n  Power|}3{| arraypower\n.~\n\n\nInventory PrimerNombre:\n  Runes nombre\n.~\n\nInventory Nombre:\n    PrimerNombre n\n.~\n\nInventory Contacto:\n  Nombre n\n  Kit of Kit of Power lista1 = << <<2,3>> , <<1,2>>  >>\n  Power|}2{| enteros1,enteros2 = |)2,3(|\n  Rune ast1 = *\\** , m1 , a1=*a*,n1=*n*,u1=*u*,ast2 = *\\**, tip\n.~\n\nboss f2():\n.~\n\nmonster f3() Contacto:\n.~\n\nmonster f(Power p11,Contacto? c11,Contacto c221,Runes c3) Contacto:\n.~\n\nworld %caso_malicia'M'%: \n\n  Rune ast1 = *\\** , m1 , a1=*a*,n1=*n*,u1=*u*,ast2 = *\\** \n\n  m1 = *M*     @ Asignacion niangara yo no me imprimo L$OL\n\n  Battle b1 = Win , b2=Lose \n  Battle b3 \n  Battle b4 = (b1 && b2) || b3,b5 = (2 > 3)? Win:Lose\n\n  Power a2 = 0,b20 = 1\n  Power c2 = a2 + 1,d2 = a2*2\n\n  Skill a3 = 1,b31 = 1'0,abc = c2 + 1,d3 = c2*2\n  Battle listab1 = <<2>> == <<>>\n  Battle listab2 = <<>> == <<>>\n  Battle listab3 = <<>> == <<2>>\n  Battle listab3'1 = <<<<>>>> == <<<<2>>>>\n\n  Battle listab4 = <<2>> == <<>>\n  Battle listab5 = (2:<<>>) == <<>>\n  Battle listab6 = <<>>::<<>> == <<>>\n  Battle listab7 = <<2>>::<<>> == <<2>>\n  Battle listab8 = <<>>::<<3>> == <<3>>\n  Battle listab8'1 = (<<2>>:<<<<3>>>>) == <<<<2>>,<<3>>>>\n  Battle listab9 = <<<<>>>> == <<<<2>>>>\n  Battle listab10 = (<<>>:<<<<2>>>>) == <<<<>>,<<2>>>>\n  Battle listab11 = <<<<>>>> == <<<<>>>>\n\n  Contacto3 nombre\n  \n  Contacto3 puff test = summon Contacto3\n  PrimerNombre2 puff test2 = summon PrimerNombre2\n  \n  puff(puff(puff test spawn pointernombre) spawn regpn) spawn entero = puff(puff(puff test spawn pointernombre) spawn regpn) spawn entero + nombre spawn arraypower|)0(|\n\n  \n  Power|}3{| hola1\n  Power|}3{| hola2\n  \n  hola1 = hola2\n  \n  test = summon Contacto3\n  \n  puff test = nombre\n  \n  nombre = nombre\n  \n  Kit of Power hola3\n  \n  hola3 = (2 + 1):hola3\n  hola3 = (hola1|)0(| + puff(puff(puff test spawn pointernombre) spawn regpn) spawn entero):hola3\n  hola3 = puff(puff(puff test spawn pointernombre) spawn regpn) spawn entero:hola3\n\n\n  \n  a3 = a3\n  \n  Power |}2{| abc2\n\n  Power|}2{| enteros1,enteros2 = |)a2,c2 + 2,(abc2|)0(| + 2 ) * (b2? a2 + 1 : abc2|)abc2|)2(| - 1(|) (|\n  \n\n  abc2|)0(| = 0\n\n  abc2|)1(| = 1\n\n  \n  Kit of Kit of Power lista1 = << <<a2,a2>> , <<a2,a2>>  >>\n  \n  lista1|>0<||>0<| = 240\n\n  \n  Kit of Power l1 = <<1,2,3,4,4>>\n  Contacto c = {<<l1,l1>>, a2}\n  c spawn n spawn n spawn nombre = joystick ~Dame un nombre YEH NOW BB~\n  @c spawn lista1|>2<||>1<| = joystick ~HEY LOL~\n  @c spawn lista1|>2<||>1<| = joystick ~HEY LOL~\n  c spawn tip = *c*    \n      \n  Power puff p = summon Power\n  \n  puff p = (b2? a2 + 1 : abc2|>3 - 1<|)\n  \n  puff p = 15\n  \n  free p\n  \n  Power|}2{| puff p2 = summon Power\n  \n  puff p2|>0<| = 21\n  \n  free|}{| p2\n  \n  Kit of Power l2 \"'= <<1,2,3,4,4>>'\" = <<>>\n  \n      \n  Kit of Power puff plista3 = summon Kit of Power @jejelol\n  Kit of Power puff plista4 = summon Kit of Power @jejelol\n  \n  puff plista3 = 2:<<>>::<<>>::<<>>::<<22>>\n  puff plista3 = (b2? a2 + 1 : abc2|>l2|>0<| - lista1|>2<||>2<|<|):<<1,2>>\n  puff plista3 = <<1,2>>\n  \n  puff plista4 = puff plista3\n\n  @ Esto deberia dar error p2|}10{| = 12\n  @ Esto deberia dar error p1 = p2|}10{|\n  @ Esto deberia dar error p1 = p2\n  \n  Power p1,p4 = 0\n  @puff p2|>-(((p1)))<| = joystick ~HEY LOL~\n\n  \n  \n  Power a = l2|>2<| + 2 + 2 * 3 // 3 / 4  % 4 - 5 - -1 + #l2\n   \n  Power b = ((a + (2)) + 2) * ((3 // 3) / (4  % 4) - 5) - ((-1) + #l2)\n  \n  Battle c22\n  c22 = ((b1 || Win && Lose || !b2 ) == Win && b4 != (b2? Win:Lose))\n  \n  Power|}2{| manu1\n  Power|}30{| manu2\n  \n  c22 = manu1 == manu1\n\n  Kit of Power manukitpower1\n  Kit of Power manukitpower2\n  \n  c22 = manukitpower1 == manukitpower2\n\n  Power puff manukitpower3\n  Kit of Kit of Power puff puff puff manukitpower4\n  \n  c22 = puff manukitpower3 == puff puff puff manukitpower4|>12<||>3<|\n       \n  Rune c1 = ^*c*\n  Rune c223 = .*c*\n\n  Rune c2222\n  Rune c3 = ^.c2222\n  \n  Rune c4 = .^c2222\n  Rune c5 = .(^c2222)\n  Rune c6 = (.(^c2222))\n  Rune c7 = ^(.(c2222))\n  \n  \n  \"' Hola '\"\n  play:\n    @<Lista de Instrucciones>\n    @test = 1\n    gameOver\n    keepPlaying\n     \n    @ Notar que la condicion de lock debe ser booleana\n    controller Power i = (2 - 2) -> 10 - 1 lock (abc2|>0<| + 2 ) * (b2? a2 + 1 : abc2|>5 - 1<|):\n\n      Button:\n        | 12 > 2 }\n          gameOver\n          keepPlaying\n          Power a\n      .~\n      Button:\n        | 1'0 > 2'2 }\n          unlock i + 2\n        | notPressed }\n          unlock i + 2\n      .~    \n    .~ \n         \n  @  asd\n  lock (abc2|>0<| + 1 ) * (b2? a2 + 1 : abc2|>2 - 1<|)\n  .~\n\n  controller Power i <- abc:\n  .~ \n  controller Skill i <- abc:\n  .~ \n  controller Contacto i <- abc:\n  .~ \n  \n  Power a222 = kill f(1,2,3,4)\n.~\n"

malicia1' :: [Token]
malicia1' = alexScanTokens malicia1

astMalicia1 :: String
astMalicia1 = "\nworld:\n  ast2 = '\''\n    u1 = 'u'\n    n1 = 'n'\n    a1 = 'a'\n    ast1 = '\''\n  m1 = 'M'\n  b2 = False\n    b1 = True\n  b5 = (2 > 3) ? True : False\n    b4 = ((b1 && b2) || b3)\n  b20 = 1\n    a2 = 0\n  d2 = (a2 * 2)\n    c2 = (a2 + 1)\n  d3 = (c2 * 2)\n    abc = (c2 + 1)\n    b31 = 1.0\n    a3 = 1\n  listab1 = ([2] == [])\n  listab2 = ([] == [])\n  listab3 = ([] == [2])\n  listab3'1 = ([[]] == [[2]])\n  listab4 = ([2] == [])\n  listab5 = ((2 : []) == [])\n  listab6 = (([] :: []) == [])\n  listab7 = (([2] :: []) == [2])\n  listab8 = (([] :: [3]) == [3])\n  listab8'1 = (([2] : [[3]]) == [[3],[2]])\n  listab9 = ([[]] == [[2]])\n  listab10 = (([] : [[2]]) == [[2],[]])\n  listab11 = ([[]] == [[]])\n  test = summon Contacto3\n  test2 = summon PrimerNombre2\n  puff (puff (puff (test) spawn pointernombre) spawn regpn) spawn entero = (puff (puff (puff (test) spawn pointernombre) spawn regpn) spawn entero + nombre spawn arraypower index: 0)\n  hola1 = hola2\n  test = summon Contacto3\n  puff (test) = nombre\n  nombre = nombre\n  hola3 = ((2 + 1) : hola3)\n  hola3 = ((hola1 index: 0 + puff (puff (puff (test) spawn pointernombre) spawn regpn) spawn entero) : hola3)\n  hola3 = (puff (puff (puff (test) spawn pointernombre) spawn regpn) spawn entero : hola3)\n  a3 = a3\n  enteros2 = [((abc2 index: 0 + 2) * b2 ? (a2 + 1) : abc2 index: (abc2 index: 2 - 1)),(c2 + 2),a2]\n  abc2 index: 0 = 0\n  abc2 index: 1 = 1\n  lista1 = [[a2,a2],[a2,a2]]\n  lista1 index: 0 index: 0 = 240\n  l1 = [4,4,3,2,1]\n  c = [a2,[l1,l1]]\n  c spawn n spawn n spawn nombre = joystick \"~Dame un nombre YEH NOW BB~\"\n  c spawn tip = 'c'\n  p = summon Power\n  puff (p) = b2 ? (a2 + 1) : abc2 index: (3 - 1)\n  puff (p) = 15\n  free p\n  p2 = summon Power\n  puff (p2) index: 0 = 21\n  free p2\n  l2 = []\n  plista3 = summon Kit of(Power)\n  plista4 = summon Kit of(Power)\n  puff (plista3) = (2 : ((([] :: []) :: []) :: [22]))\n  puff (plista3) = (b2 ? (a2 + 1) : abc2 index: (l2 index: 0 - lista1 index: 2 index: 2) : [2,1])\n  puff (plista3) = [2,1]\n  puff (plista4) = puff (plista3)\n  p4 = 0\n  a = (((((l2 index: 2 + 2) + ((((2 * 3) // 3) / 4) % 4)) - 5) - -1) + #l2)\n  b = ((((a + 2) + 2) * (((3 // 3) / (4 % 4)) - 5)) - (-1 + #l2))\n  c22 = ((((b1 || (True && False)) || !b2) == True) && (b4 != b2 ? True : False))\n  c22 = (manu1 == manu1)\n  c22 = (manukitpower1 == manukitpower2)\n  c22 = (puff (manukitpower3) == puff (puff (puff (manukitpower4))) index: 12 index: 3)\n  c1 = ^'c'\n  c223 = .'c'\n  c3 = ^.c2222\n  c4 = .^c2222\n  c5 = .^c2222\n  c6 = .^c2222\n  c7 = ^.c2222\n  While ((abc2 index: 0 + 1) * b2 ? (a2 + 1) : abc2 index: (2 - 1)):\n      GameOver\n      KeepPlaying\n      For i = (2 - 2) -> (10 - 1) while: ((abc2 index: 0 + 2) * b2 ? (a2 + 1) : abc2 index: (5 - 1)):\n      IF:\n    (12 > 2) }\n      KeepPlaying\n  GameOver\n      IF:\n    (1.0 > 2.2) }\n      unlock (i + 2)(1.0 > 2.2) }\n      unlock (i + 2)\n\n  ForEach i <- abc:\n\n  ForEach i <- abc:\n\n  ForEach i <- abc:\n\n  a222 = kill f(1,2,3,4)\n"

ricardo :: String
ricardo = "monster n(Power n) Power:\n  @Skill n @Comentando esto deberia compilar\n  play:\n    Battle n\n    drop n  @ verificar si esta inicializada?\n  lock Win\n  .~\n  unlock n\n.~\n\nworld %n%:\n  Power n\n  n = kill n(n) @Falla con verificacion de tipos\n.~"

ricardo' :: [Token]
ricardo' = alexScanTokens ricardo

astRicardo :: String
astRicardo = "\nworld:\n  n = kill n(n)\n"

factorial :: String
factorial = "monster factorial(Power n) Power:\n  Button:\n    | n < 0 } unlock 0\n    | n == 0 } unlock 1\n    | n > 0 } unlock n * kill factorial(n-1)\n  .~\n.~\n\nworld %Factorial%:\n  Power f = kill factorial(10)\n  Skill factorial.~"

factorial' :: [Token]
factorial' = alexScanTokens factorial

astFactorial :: String
astFactorial = "\nworld:\n  f = kill factorial(10)\n"

factorialIterativo :: String
factorialIterativo = "world %Caso2%:\n  Power numero = joystick ~Por favor dime un numero: ~\n  Power factorial = 1\n  play:\n    factorial = factorial * numero\n    numero--\n  lock numero > 1\n  .~\n  Button:\n    | numero > 0 }\n      drop ~El factorial del numero indicado es ~, factorial\n    | numero < 0 }\n      drop ~El factorial de un numero negativo no esta definido, saludos.~\n  .~\n  drop ~Nos veremos pronto.~\n.~\n"

factorialIterativo' :: [Token]
factorialIterativo' = alexScanTokens factorialIterativo

astFactorialIterativo :: String
astFactorialIterativo = "\nworld:\n  numero = joystick \"~Por favor dime un numero: ~\"\n  factorial = 1\n  While (numero > 1):\n      factorial = (factorial * numero)\n      numero = (numero - 1)\n\n  IF:\n    (numero > 0) }\n      drop [factorial,\"~El factorial del numero indicado es ~\"](numero < 0) }\n      drop [\"~El factorial de un numero negativo no esta definido, saludos.~\"]  drop [\"~Nos veremos pronto.~\"]\n"

fibonnaci :: String
fibonnaci = "boss fibonnaci(Power primero, Power segundo, Power limite):\n  drop primero\n  drop segundo\n  Power n = primero\n  play:\n    drop primero + segundo\n    primero = segundo\n    segundo = n + segundo\n    limite--\n  lock limite > 0\n  .~\n.~\n\nworld %Caso9%:\n  drop ~Fibonnaci: ~\n  kill fibonnaci(1, 2, 5)\n.~"

fibonnaci' :: [Token]
fibonnaci' = alexScanTokens fibonnaci

astFibonnaci :: String
astFibonnaci = "\nworld:\n  drop [\"~Fibonnaci: ~\"]  kill fibonnaci(1,2,5)\n"

fragmentos :: String
fragmentos = "Inventory Registro:\n  Power up\n  Rune tip\n.~\nInventory Contacto:\n  Runes nombre\n  Power edad\n  Battle tieneTrabajo\n.~\nInventory Circle:\n  Skill centerX\n  Skill centerY\n  Skill radius\n.~\nInventory Rectangle:\n  Skill topLeftX\n  Skill topLeftY\n  Skill bottomRightX\n  Skill bottomRightY\n.~\nInventory Producto:\n  Power precioReal\n.~\nItems Shape:\n  Circle c\n  Rectangle r\n.~\nmonster areaCirculo(Circle ?c) :\n  unlock 3'14 * c spawn radius * c spawn radius\n.~\nmonster calcularGanancia(Power precioComprado, Power precioVendido) Power:\n  unlock precioVendido - precioComprado\n.~\nmonster calcularGanancia2(Producto ?producto, Power precioVendido) Power:\n  unlock precioVendido - producto spawn precioReal\n.~\n\nworld %FragmentosDeCodigo%:\n  Power a, b\n  a = 0001\n  b = 003\n  Battle esMayor = Lose\n  a = 1\n  Skill r = 0'5\n  \n  Rune|}3{| abc\n  Runes|}3{| nombres = |)~Natascha~, ~Francisco~, ~Manuel~(|\n  Skill|}3{| indices = |)3'67, 3'20, 3'0(|\n  Kit of Rune lista1 = <<*C*, *3*>>\n  \n  Registro r2 = {0, *f*}\n  Contacto alex = {~Alex~, 15, Lose}\n  Contacto sofia\n  sofia spawn nombre = ~Sofia~\n  sofia spawn edad = 29\n  sofia spawn tieneTrabajo = Win\n  drop ~Hola ~, sofia spawn nombre\n\n  Shape sh\n  sh spawn c spawn centerX = 2'1\n  sh spawn c spawn centerY = 5'0\n  sh spawn c spawn centerY = 5'0\n  sh spawn c spawn radius = 15\n  drop ~el area del circulo es ~, kill areaCirculo(sh spawn c)\n\n  Power|}5{| puff p1 = summon Power\n  puff p1|)1(| = 15\n  free|}{| p1\n\n  Power puff p2 = summon Power\n  puff p2 = 15\n  free p2\n\n  Power n1 = 15, n2 = 13\n  n1++\n  n2--\n  Power n3 = n1, n4 = n2\n\n  a = a > b ? Win : Lose\n\n  Battle puedeConducir = Lose\n  Power edad = 18\n  Button:\n  | edad >= 18 }\n    puedeConducir = Win\n  .~\n\n  Power i = 0\n\n  Runes|}5{| edades = |)~12~, ~23~, ~15~, ~40~, ~15~(|\n  Runes|}5{| nombres2 = |)~Natascha~, ~Francisco~, ~Manuel~, ~Ricardo~, ~Haskell~(|\n  controller i = 0 -> 4:\n    drop ~Hola\n~\n    drop nombres|)i(|, ~ tienes ~, edad|)i(|, ~ anios!~\n  .~\n\n  play:\n    drop ~Hola ~, nombres|)i(|, ~ tienes ~, edad|)i(|, ~ anios!~\n    i++\n  lock i < 5\n  .~\n\n  drop ~Ganancia de ~, kill calcularGanancia(1500, 2000)\n\n  Producto p3\n  p3 spawn precioReal = 1500\n  drop ~Ganancia de ~, kill calcularGanancia2(puff p3, 2000)\n.~\n"

fragmentos' :: [Token]
fragmentos' = alexScanTokens fragmentos

astFragmentos :: String
astFragmentos = "\nworld:\n  a = 1\n  b = 3\n  esMayor = False\n  a = 1\n  r = 0.5\n  nombres = [\"~Manuel~\",\"~Francisco~\",\"~Natascha~\"]\n  indices = [3.0,3.2,3.67]\n  lista1 = ['3','C']\n  r2 = ['f',0]\n  alex = [False,15,\"~Alex~\"]\n  sofia spawn nombre = \"~Sofia~\"\n  sofia spawn edad = 29\n  sofia spawn tieneTrabajo = True\n  drop [sofia spawn nombre,\"~Hola ~\"]  sh spawn c spawn centerX = 2.1\n  sh spawn c spawn centerY = 5.0\n  sh spawn c spawn centerY = 5.0\n  sh spawn c spawn radius = 15\n  drop [kill areaCirculo(sh spawn c),\"~el area del circulo es ~\"]  p1 = summon Power\n  puff (p1) index: 1 = 15\n  free p1\n  p2 = summon Power\n  puff (p2) = 15\n  free p2\n  n2 = 13\n    n1 = 15\n  n1 = (n1 + 1)\n  n2 = (n2 - 1)\n  n4 = n2\n    n3 = n1\n  a = (a > b ? True : False)\n  puedeConducir = False\n  edad = 18\n  IF:\n    (edad >= 18) }\n      puedeConducir = True\n  i = 0\n  edades = [\"~15~\",\"~40~\",\"~15~\",\"~23~\",\"~12~\"]\n  nombres2 = [\"~Haskell~\",\"~Ricardo~\",\"~Manuel~\",\"~Francisco~\",\"~Natascha~\"]\n  For i = 0 -> 4:\n      drop [\"~Hola\\n~\"]      drop [\"~ anios!~\",edad index: i,\"~ tienes ~\",nombres index: i]\n  While (i < 5):\n      drop [\"~ anios!~\",edad index: i,\"~ tienes ~\",nombres index: i,\"~Hola ~\"]      i = (i + 1)\n\n  drop [kill calcularGanancia(1500,2000),\"~Ganancia de ~\"]  p3 spawn precioReal = 1500\n  drop [kill calcularGanancia2(puff (p3),2000),\"~Ganancia de ~\"]\n"

hola :: String
hola = "world %HolaMundo%:  drop ~HELLO WORLD!~.~"

hola' :: [Token]
hola' = alexScanTokens hola

astHola :: String
astHola = "\nworld:\n  drop [\"~HELLO WORLD!~\"]\n"

parOImpar :: String
parOImpar = "world %ParOImpar%:\n  \"' Dice si un numero es par o impar '\"\n  Power numero\n  numero = joystick ~Introduzca un numero entero: ~\n  Button:\n  |numero % 2 == 0 }\n    drop ~es par~\n  | notPressed }\n    drop ~es impar~\n  .~\n.~\n"

parOImpar' :: [Token]
parOImpar' = alexScanTokens parOImpar

astParOImpar :: String
astParOImpar = "world:\n  numero = joystick \"~Introduzca un numero entero: ~\"\n  IF:\n    ((numero % 2) == 0) }\n      drop [\"~es par~\"]True }\n      drop [\"~es impar~\"]"

treeInOrder :: String
treeInOrder = "\"'\nPrintArbol.game\n\nCrea un arbol utilizando registros, punteros, y subrutinas.\n\nLuego lo imprime en forma in-order : el nodo izquierdo | nodo central |  nodo derecho\n\n'\"\n\n\nInventory Nodo:\n  \"'Representa un nodo del arbol'\"\n  Nodo puff izqui  @ Puntero al nodo izquierdo\n  Nodo puff dere   @ Puntero al nodo derecho\n  Power puff dato  @ Puntero al dato de este nodo\n.~\n\nboss iniciar(Nodo? nodo,Power dato):\n  \"'Inicia un nodo con un valor'\"\n  nodo spawn izqui = DeathZone\n  nodo spawn dere = DeathZone\n  nodo spawn dato = summon Power\n  puff(nodo spawn dato) = dato\n\n.~\n\nboss print(Nodo? nodo):\n  \"'Imprime el arbol in-order'\"\n\n  Button:\n  | nodo spawn izqui == DeathZone } \n    kill print(nodo spawn izqui)\n  .~\n\n  drop nodo spawn dato\n\n  Button:\n  | nodo spawn dere == DeathZone } \n    kill print(nodo spawn dere)\n  .~\n\n.~\n\nboss insert(Nodo? nodo, Power dato):\n  \"'Inserta un valor en el'\"\n\n  Button:\n  | nodo spawn dato != DeathZone } @ Si el nodo tiene un valor\n\n    Power dato_n = puff (nodo spawn dato)\n\n    Button:\n    | dato < dato_n }  @ Si el valor a insertar es MENOR al valor de este nodo\n      Button:\n      | nodo spawn izqui == DeathZone } \n        nodo spawn izqui = summon Nodo\n        kill iniciar(puff nodo spawn izqui,dato)\n      | notPressed }\n        kill insert(puff nodo spawn izqui,dato)\n      .~\n    | dato > dato_n }  @ Si el valor a insertar es MAYOR al valor de este nodo\n      Button:\n      | nodo spawn dere == DeathZone } \n        nodo spawn dere = summon Nodo\n        kill iniciar(puff nodo spawn dere,dato)\n      | notPressed }\n        kill insert(puff nodo spawn dere,dato)\n      .~\n    .~\n  | notPressed }  @ Si el nodo no tiene un valor\n    kill iniciar(puff nodo,dato)\n  .~\n.~\n\nworld %tree%:\n\n  Nodo root \n  Nodo puff root1\n  root = {}\n  @root = summon Nodo @Da error con ckecktypes\n  root1 = summon Nodo\n\n  kill iniciar(root,10)\n  kill insert(root,6)\n  kill insert(root,14)\n  kill insert(root,3)\n\n  kill print(root)\n\n.~\n"

treeInOrder' :: [Token]
treeInOrder' = alexScanTokens treeInOrder

astTreeInOrder :: String
astTreeInOrder = "\nworld:\n  root = []\n  root1 = summon Nodo\n  kill iniciar(root,10)\n  kill insert(root,6)\n  kill insert(root,14)\n  kill insert(root,3)\n  kill print(root)\n"

declAsng :: String
declAsng = "Inventory Trimestre:\n  Battle inscripcion_ok\n  Power uc\n  Skill indice\n  Runes estudiante\n.~\n\nItems Estudiante:\n  Battle f\n  Power v\n.~\n\nworld %Caso6%:\n  Power int = 2\n  Skill float = 1'5\n  Rune char = .(^*c*)\n  Runes string = ~Hola Mundo!~\n  Battle b = (Win && Lose) == Lose\n  Runes|}int{| r = |)*H*,*i*(|\n  Kit of Skill l = <<2'7999, 2'8000, 2'9999, 3'0000, 4'2500, 4'2501, 4'7499, 4'7500, 5'0000>>\n  Trimestre abril_Julio_2019 = { Lose, 0, 4'1416, ~12-11163~ }\n  Estudiante estudiante'\n  estudiante' spawn f = Win\n  Power puff p = summon Trimestre\n.~"

declAsng' :: [Token]
declAsng' = alexScanTokens declAsng

astDeclAsng :: String
astDeclAsng = "\nworld:\n  int = 2\n  float = 1.5\n  char = .^'c'\n  string = \"~Hola Mundo!~\"\n  b = ((True && False) == False)\n  r = ['H','i']\n  l = [2.7999,2.8,2.9999,3.0,4.25,4.2501,4.7499,4.75,5.0]\n  abril_Julio_2019 = [False,0,4.1416,\"~12-11163~\"]\n  estudiante' spawn f = True\n  p = summon Trimestre\n"

registros :: String
registros = "Inventory PotionsHealth:\n  Power p\n.~\nInventory PotionsHealth2:\n  Power p\n.~\nInventory PotionsMana:\n  Skill p\n.~\nInventory PotionsMana2:\n  Skill p\n.~\n\nworld %registros%:\n  PotionsHealth ph\n  PotionsMana   pm\n.~"

registros' :: [Token]
registros' = alexScanTokens registros

astRegistros :: String
astRegistros = "\nworld:\n\n"

subrutinas :: String
subrutinas = "boss b():\n  drop ~Nada~\n.~\n\nmonster m() Power:\n  Power i = 0\n  unlock i\n.~\n\nworld %subrutinas%:\n  Power m = kill m()\n  kill b() @b no es una func\n.~"

subrutinas' :: [Token]
subrutinas' = alexScanTokens subrutinas

astSubrutinas :: String
astSubrutinas = "\nworld:\n  m = kill m()\n  kill b()\n"

tablas :: String
tablas = "world %Tablas%:\n  Rune seguir = *s*\n  Power i, numero\n  \n  play:\n    numero = joystick ~Introduzca un numero entero: ~\n  \n    drop ~La tabla de multiplicar del ~,numero,~ es:~\n  \n    @ Inicio del anidamiento\n    i = 1 \n    play:\n      drop numero, ~ \\* ~, i, ~ = ~, i * numero\n      i++\n    lock i <= 10\n    .~\n    @ Fin del anidamiento\n  \n    seguir = kill portalRunesToRune( joystick ~Desea ver otra tabla (s/n)?: ~)\n  lock  seguir != *n*\n  .~\n.~\n"

tablas' :: [Token]
tablas' = alexScanTokens tablas

astTablas :: String
astTablas = "\n\n"

volumen :: String
volumen = "world %VolumenCubo%:\n  \"' Calcula el volumen de un Cubo '\"\n  Skill arista, volumen\n  \n  arista = joystick ~Introduzca arista: ~\n  \n  volumen = arista * arista * arista\n  \n  drop ~el volumen del cubo es: ~, volumen\n.~\n"

volumen' :: [Token]
volumen' = alexScanTokens volumen

astVolumen :: String
astVolumen = "\nworld:\n  arista = joystick \"~Introduzca arista: ~\"\n  volumen = ((arista * arista) * arista)\n  drop [\"~el volumen del cubo es: ~\",volumen]\n"

wrong :: String
wrong = "world %Caso3%:\n  world = 2\n.~"

wrong' :: [Token]
wrong' = alexScanTokens wrong

wrongProg :: String
wrongProg = "world %Caso 4%:\n  drop ~Esto es incorrecto, no compilara~\n.~"

wrongProg' :: [Token]
wrongProg' = alexScanTokens wrongProg

emptyProgName :: String
emptyProgName = "world %%:\n  drop ~~\n.~"

emptyProgName' :: [Token]
emptyProgName' = alexScanTokens emptyProgName