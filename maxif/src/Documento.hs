module Documento
  ( Doc,
    vacio,
    linea,
    texto,
    foldDoc,
    (<+>),
    indentar,
    mostrar,
    imprimir,
  )
where

data Doc
  = Vacio
  | Texto String Doc
  | Linea Int Doc
  deriving (Eq, Show)

vacio :: Doc
vacio = Vacio

linea :: Doc
linea = Linea 0 Vacio

texto :: String -> Doc
texto t | '\n' `elem` t = error "El texto no debe contener saltos de línea"
texto [] = Vacio
texto t = Texto t Vacio

-- foldDoc :: ... PENDIENTE: Ejercicio 1 ...
--foldDoc = error "PENDIENTE: Ejercicio 1"

foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
foldDoc cVacio cTexto cLinea doc = case doc of
                            Vacio -> cVacio
                            Texto t doc' -> cTexto t (rec doc') 
                            Linea i doc' -> cLinea i (rec doc')
        where rec = foldDoc cVacio cTexto cLinea 


-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>

(<+>) :: Doc -> Doc -> Doc
(Texto t1 Vacio) <+> (Texto t2 Vacio) = texto (t1++t2)
d1 <+> d2 = foldDoc (d2) (\t rec -> Texto t rec) (\i rec -> Linea i rec) d1

-- ejemplo
-- d1 = texto "a" <+> linea <+> texto "b"
-- texto "a" <+> (Linea 0 Vacio <+> Texto "b" Vacio) (la segunda parte del concat queda guardad en cVacio)
-- texto "a" <+> (Linea 0 (Texto "b" Vacio))
-- Texto "a" Vacio <+> (Linea 0 (Texto "b" Vacio))
-- Texto "a" (fold f g h Vacio)
-- Texto "a" (Linea 0 (Texto "b" Vacio))


indentar :: Int -> Doc -> Doc
indentar i = foldDoc (Vacio) (\t rec -> Texto t rec) (\t rec -> Linea (t + i) rec)



mostrar :: Doc -> String
mostrar = error "PENDIENTE: Ejercicio 4"

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
