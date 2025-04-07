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
pponObjetoSimple ppon = case ppon of
                          TextoPP t -> False
                          IntPP n -> False
                          ObjetoPP hijos -> foldr ((&&) . pponAtomico . snd) True hijos

intercalar :: Doc -> [Doc] -> Doc
intercalar separador = foldr (\x rec -> if rec == vacio then x else (x <+> separador) <+> rec) vacio
--intercalar = error "PENDIENTE: Ejercicio 8"
-- intercalar (texto ", ") [texto "a", texto "b", texto "c"]) ⇝"a, b, c"
-- intercalar (texto ", ") [texto "a", Linea 0 Vacio, texto "b"]) ⇝   ("a, \n, "b")    ("a\nb")

-- foldr1 f [texto "a", texto "b"]
-- f texto "a" (foldr1 f [texto "b"])
-- f texto "a" (texto "b")
-- texto "a" <+> texto "," <+> linea <+> texto "b"
-- texto "a" <+> (texto "," <+> (linea <+> texto "b"))
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

formatearObjeto :: [(String, PPON)] -> Doc
formatearObjeto obj = if pponObjetoSimple (ObjetoPP obj) then
                            texto "{ "
                            <+> intercalar (texto ", ") (map (\(k, v) -> texto (show k) <+> texto ": " <+> pponADoc v) obj)
                            <+> texto " }"
                          else
                            entreLlaves (map (\(c, v) -> texto (show c) <+> texto ": " <+> pponADoc v) obj)

pponADoc :: PPON -> Doc
pponADoc p = case p of
          TextoPP s -> texto (show s)
          IntPP i -> texto (show i)
          ObjetoPP obj -> formatearObjeto obj
