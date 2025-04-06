module PPON where

import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

pponAtomico :: PPON -> Bool
pponAtomico ppon = case ppon of
                    TextoPP s -> True
                    IntPP n -> True
                    ObjetoPP o -> False


--devuelve True si el PPON es construido con ObjetoPP Y ademas todos sus sub-objetos son atomicos
pponObjetoSimple :: PPON -> Bool
pponObjetoSimple (TextoPP t) = False
pponObjetoSimple (IntPP n) = False
pponObjetoSimple (ObjetoPP hijos) = foldr (\x rec -> (&&) (pponAtomico (snd x)) rec) True hijos


intercalar :: Doc -> [Doc] -> Doc
--intercalar separador = foldr (\x rec -> ) Vacio
intercalar = error "PENDIENTE: Ejercicio 8"
-- intercalar (texto ", ") [texto "a", texto "b", texto "c"]) ⇝"a, b, c"


entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds =
  texto "{"
    <+> indentar
      2
      ( linea
          <+> intercalar (texto "," <+> linea) ds
      )
    <+> linea
    <+> texto "}"

aplanar :: Doc -> Doc
aplanar = error "PENDIENTE: Ejercicio 8"

pponADoc :: PPON -> Doc
pponADoc = error "PENDIENTE: Ejercicio 9"
