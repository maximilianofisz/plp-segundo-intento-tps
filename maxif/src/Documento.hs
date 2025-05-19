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
d1 <+> d2 = foldDoc d2
                    (\t rec -> case rec of
                              Vacio -> texto t
                              Texto t' rec' -> Texto (t ++ t') rec'
                              Linea i rec' -> Texto t rec
                    )
                    Linea
                    d1
-- <+> respeta el invariante de Doc porque:
--    Si d1 = vacío: se devuelve d2 que ya era un documento válido.
--    Si d1 = Texto t rec:
--        Si rec = Vacío: se crea un nuevo texto con el contenido de t. La función 'texto' garantiza que 't' no sea vacía y que no contenga saltos de línea.  
--        Si rec = Texto t' rec': (t ++ t') respeta el invariante porque ninguno es un string vacío ni contienen saltos de línea. Como rec es un Texto, unimos t y t' para cumplir con que la subestructura de un Texto s d debe ser Vacio o Linea i d'. FIX: Aclara invariante texto t (texto t' ...)
--        Si rec = Linea i rec': lo convertimos a Texto. Ya sabemos que 't' no tiene saltos de línea y rec es válido.
--    Si d1 = Linea i rec: lo procesamos de forma que d2 contenga una nueva Linea. Al usar 'Linea' respetamos que i >= 0. rec y d2 ya son documentos válidos.


indentar :: Int -> Doc -> Doc
indentar i = foldDoc Vacio Texto (\t rec -> Linea (t + i) rec)
-- indentar respeta el invariante de Doc porque:
--    Si el doc es vacío se devuelve el caso vacío del foldDoc ---> Vacio.
--    Si es Texto s d: al usar el constructor 'Texto' a secas, su argumento no se modifica (ya cumplia el invariante) y el resto del documento se va procesando recursivamente respetando el invariante. FIX: no se expresaba correctamente por que el caso respetaba inv
--    Si es Linea i d: (t + i) >= 0 ya que i >= 0 y t >= 0. Luego el resto del documento se procesa recursivamente respetando el invariante. 


mostrar :: Doc -> String
mostrar = foldDoc "" (++) (\i rec -> "\n" ++ replicate i  ' ' ++ rec) -- FIX: crear los whitespaces directamente con replicate


-- | Función dada que imprime un documento en pantalla

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
