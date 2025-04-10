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

-- devuelve True si y solo si el PPON es construido con ObjetoPP Y ademas todos sus sub-objetos son pponAtomicos
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
-- Un ObjetoPP se procesa como simple o compuesto recursivamente
formatearObjeto :: [(String, PPON)] -> Doc
formatearObjeto hijos = if pponObjetoSimple (ObjetoPP hijos)
                          then -- Simple
                            texto "{ "
                            <+> intercalar (texto ", ") (formatearHijos hijos) 
                            <+> texto " }"
                          else -- Compuesto
                            entreLlaves (formatearHijos hijos)
          where formatearHijos = map (\(k, v) -> texto (show k) <+> texto ": " <+> pponADoc v)


-- El esquema de recursión es primitiva.
-- Para los casos bases de la recursión (constructor textoPP o IntPP se devuelve un valor "fijo" (técnicamente fijo el show pero dependiente del argumento))
-- En el caso recursivo, durante el llamado a la recursión sobre el resto de la estructura se utiliza solamente la subestructura correspondiente (pponAdoc v) sin modificarla. Todo esto indicaría que la recursión es estructural,
-- sin embargo, en el caso recursivo se interactúa con las subestructuras cuando se decide si el objeto es simple o no (if pponObjetoSimple (ObjetoPP hijos) donde se recorre cada uno de los hijos) por lo que finalmente es primitiva.
pponADoc :: PPON -> Doc
pponADoc p = case p of
          TextoPP s -> texto (show s)
          IntPP i -> texto (show i)
          ObjetoPP hijos -> formatearObjeto hijos
