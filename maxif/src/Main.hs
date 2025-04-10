module Main (main) where

import Documento
import PPON
import Test.HUnit

main :: IO ()
main = runTestTTAndExit allTests

allTests :: Test
allTests =
  test
    [ "Ejercicio 2" ~: testsEj2,
      "Ejercicio 3" ~: testsEj3,
      "Ejercicio 4" ~: testsEj4,
      "Ejercicio 6" ~: testsEj6,
      "Ejercicio 7" ~: testsEj7,
      "Ejercicio 8" ~: testsEj8,
      "Ejercicio 9" ~: testsEj9
    ]

testsEj2 :: Test
testsEj2 =
  test
    [ vacio <+> vacio ~?= vacio,
      vacio <+> texto "no vacio" ~?= texto "no vacio",
      texto "no vacio" <+> vacio ~?= texto "no vacio",
      texto "a" <+> texto "b" ~?= texto "ab",
      mostrar (texto "a" <+> linea) ~?= "a\n",
      mostrar (linea <+> texto "a" ) ~?= "\na",
      (texto "a" <+> linea) <+> texto "b" ~?= texto "a" <+> (linea <+> texto "b")
    ]

testsEj3 :: Test
testsEj3 =
  test
    [ indentar 2 vacio ~?= vacio,
      indentar 2 (texto "a") ~?= texto "a",
      indentar 2 (texto "a" <+> linea <+> texto "b") ~?= texto "a" <+> indentar 2 (linea <+> texto "b"),
      indentar 2 (linea <+> texto "a") ~?= indentar 1 (indentar 1 (linea <+> texto "a")),
      mostrar (indentar 1 (linea <+> texto "a")) ~?= "\n a"
    ]

testsEj4 :: Test
testsEj4 =
  test
    [ mostrar vacio ~?= "",
      mostrar linea ~?= "\n",
      mostrar (linea <+> linea) ~?= "\n\n",
      mostrar (texto "a" <+> linea <+> texto "b") ~?= "a\nb" ,
      mostrar (vacio <+> texto "a" <+> vacio <+> texto "b") ~?= "ab",
      mostrar (indentar 2 (texto "a" <+> linea <+> texto "b")) ~?= "a\n  b"
    ]

pericles, merlina, addams, familias :: PPON
pericles = ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]
merlina = ObjetoPP [("nombre", TextoPP "Merlina"), ("edad", IntPP 24)]
addams = ObjetoPP [("0", pericles), ("1", merlina)]
familias = ObjetoPP [("Addams", addams)]

testsEj6 :: Test
testsEj6 =
  test
    [ pponObjetoSimple pericles ~?= True,
      pponObjetoSimple addams ~?= False,
      pponObjetoSimple (ObjetoPP [("nombre", TextoPP "Merlina"), ("0", pericles)]) ~?= False,
      pponObjetoSimple (TextoPP "hola") ~?= False,
      pponObjetoSimple (IntPP 1337) ~?= False,
      pponObjetoSimple familias ~?= False
    ]

a, b, c :: Doc
a = texto "a"
b = texto "b"
c = texto "c"

testsEj7 :: Test
testsEj7 =
  test
    [ mostrar (intercalar (texto ", ") []) ~?= "",
      mostrar (intercalar (texto ", ") [a, b, c]) ~?= "a, b, c",
      mostrar (intercalar vacio [a,b,c]) ~?= "abc",
      mostrar (intercalar linea [a,b,c]) ~?= "a\nb\nc",
      mostrar (intercalar (texto ";" <+> linea) [a,b,c]) ~?= "a;\nb;\nc",
      mostrar (entreLlaves []) ~?= "{ }",
      mostrar (entreLlaves [a, b, c]) ~?= "{\n  a,\n  b,\n  c\n}",
      mostrar (entreLlaves [vacio,vacio,vacio]) ~?= "{\n  \n}",
      mostrar (entreLlaves [vacio,vacio]) ~?= "{\n  \n}",
      mostrar (entreLlaves [linea, linea]) ~?= "{\n  \n  ,\n  \n  \n}"
    ]

testsEj8 :: Test
testsEj8 =
  test
    [ mostrar (aplanar (a <+> linea <+> b <+> linea <+> c)) ~?= "a b c",
      mostrar (aplanar vacio) ~?= "",
      mostrar (aplanar (texto "a" <+> texto "b" <+> texto "c")) ~?= "abc",
      mostrar (aplanar (indentar 100 (texto "a" <+> linea <+> texto "b"))) ~?= "a b",
      mostrar (aplanar (entreLlaves [a, b, c])) ~?= "{ a, b, c }",
      mostrar (aplanar (entreLlaves [vacio])) ~?= "{  }"
    ]


simple1 = ObjetoPP [("test1", IntPP 1), ("test2", IntPP 2), ("test3", IntPP 3)]
compuestoAnidado = ObjetoPP [("Estoy anidado?", TextoPP "si")]
compuestoAux = ObjetoPP [("mi unico y preciado hijo", compuestoAnidado)]
simple2 = ObjetoPP [("testA", TextoPP "a"), ("testB", TextoPP "b"), ("testC", TextoPP "c")]
compuestoPrincipal = ObjetoPP [("primero", simple1), ("segundo", compuestoAux), ("tercero", simple2)]
anidar = ObjetoPP [("down", ObjetoPP [("the", ObjetoPP [("rabbit", ObjetoPP [("hole", ObjetoPP [("los", TextoPP "simples"), ("vamos", TextoPP "en"), ("mismo", TextoPP "renglon")])])])])]

testsEj9 :: Test
testsEj9 =
  test
    [ mostrar (pponADoc (TextoPP "hola")) ~?= "\"hola\"",
      mostrar (pponADoc (IntPP 1234)) ~?= "1234",
      mostrar (pponADoc (ObjetoPP [])) ~?= "{  }",
      mostrar (pponADoc pericles) ~?= "{ \"nombre\": \"Pericles\", \"edad\": 30 }",
      mostrar (pponADoc addams) ~?= "{\n  \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n  \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n}",
      mostrar (pponADoc familias) ~?= "{\n  \"Addams\": {\n    \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n    \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n  }\n}",
      mostrar (pponADoc compuestoPrincipal) ~?= "{\n  \"primero\": { \"test1\": 1, \"test2\": 2, \"test3\": 3 },\n  \"segundo\": {\n    \"mi unico y preciado hijo\": { \"Estoy anidado?\": \"si\" }\n  },\n  \"tercero\": { \"testA\": \"a\", \"testB\": \"b\", \"testC\": \"c\" }\n}",
      mostrar (pponADoc anidar) ~?= "{\n  \"down\": {\n    \"the\": {\n      \"rabbit\": {\n        \"hole\": { \"los\": \"simples\", \"vamos\": \"en\", \"mismo\": \"renglon\" }\n      }\n    }\n  }\n}"
    ]
