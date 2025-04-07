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

--devuelve True si y solo si el PPON es construido con ObjetoPP Y ademas todos sus sub-objetos son pponAtomicos
pponObjetoSimple :: PPON -> Bool
pponObjetoSimple ppon = case ppon of
                          TextoPP t -> False
                          IntPP n -> False
                          ObjetoPP hijos -> foldr ((&&) . pponAtomico . snd) True hijos

intercalar :: Doc -> [Doc] -> Doc
intercalar separador = foldr (\x rec -> if rec == vacio then x else (x <+> separador) <+> rec) vacio


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
aplanar = foldDoc vacio (\t rec -> texto t <+> rec) (\i rec -> texto " " <+> rec)


-- Aux principal de pponADoc
-- Un ObjetoPP se procesa como simple o "compuesto" recursivamente
formatearObjeto :: [(String, PPON)] -> Doc
formatearObjeto hijos = if pponObjetoSimple (ObjetoPP hijos)
                          then --simple
                            texto "{ "
                            <+> intercalar (texto ", ") (ctor hijos) 
                            <+> texto " }"
                          else --comp
                            entreLlaves (ctor hijos)
          where ctor = map (\(k, v) -> texto (show k) <+> texto ": " <+> pponADoc v)


-- El esquema de recursion es global, ya que en formatearObjeto se accede directamente a la recursion de una subestructura de hijos,
-- espeficifamente cuando concatenamos el resultado de pponADoc v.
pponADoc :: PPON -> Doc
pponADoc p = case p of
          TextoPP s -> texto (show s)
          IntPP i -> texto (show i)
          ObjetoPP hijos -> formatearObjeto hijos
