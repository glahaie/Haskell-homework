{-*****************************************************************************
* Cours		: INF2160
* Session	: Hiver 2013
* Objet		: Travail pratique 1
* Titre		: Module de gestion d'un agencement
* 
* Auteur	: Bernard Lefebvre
* 
* Travail effectu� par Guillaume Lahaie
*                      LAHG04077707
* Derni�re modification: 1er mars 2013
*
* Le fichier contient la d�finition des fonctions lesIds, lesComposantsDuType,
* blocIsole et connecterUn. J'ai aussi d�fini une fonction composantsCommun, qui
* permet de v�rifier si deux blocs ont un composant en commun. Pour la fonction
* connecterUn, j'utilise la fonction Find qui retourne un resultat Maybe. Pour
* travailler avec ce r�sultat, j'utilise les fonctions du module Maybe, et
* donc je l'importe.
*******************************************************************************
-}

module Agencement where

import Data.List 
import Data.Maybe

data DIRECTION = Droite | Gauche
                 deriving (Eq,Show,Read,Ord)

data AXE = X | Y
	   deriving (Eq,Show,Read,Ord)

data TYPECOMPOSANT = Bas | Haut | Coin | Frigidaire | LaveVaisselle | Four | 
                     TableCuisson | MicroOnde | Hotte | Cuisiniere
	    deriving (Eq,Show,Read,Ord)

-- Une dimension s'exprime par un triplet (hauteur, lorgeur, profondeur)
-- type DIMENSION = (Num,Num,Num)
type DIMENSION = (Float,Float,Float)

-- Le type POSITION est relatif � la position d'un composant dans la pi�ce en
-- fonction des coordonn�es (x, y, z) de son coin inf�rieur gauche, le 
-- constructeur Indetermine est relatif � une position non d�finie
data POSITION = Position {x, y, z::Float} | Indetermine
	        deriving (Eq,Show,Read,Ord)

data COMPOSANT = Meuble {lIdComposant::String, laDimension::DIMENSION, 
                         laPos::POSITION, leType::TYPECOMPOSANT} |
                 Electro {lIdComposant::String, laDimension::DIMENSION, 
                          laPos::POSITION, leType::TYPECOMPOSANT}
	         deriving (Eq,Show,Read,Ord)

-- Le type AXCMP permet de situer un composant le long d'un axe.
-- Pour ce nouveau type on d�rive l'appartenance aux classes Read et Show
-- seulement
newtype AXCMP = AxCmp {leCouple::(AXE,COMPOSANT)}
    deriving (Show,Read)

-- On d�finit l'appartenance de AXCMP � la classe Eq, seul le second �l�ment
-- du couple sert � d�terminer l'�galit�, les couples sont �gaux si et seulement
-- si les composants sont egaux
instance Eq AXCMP where
    AxCmp c1 == AxCmp c2 = snd c1 == snd c2

-- Pour ce nouveau type, on d�rive l'appartenance aux classes Show et Read.
newtype BLOC = Bloc {lesElems::[AXCMP]}
    deriving (Show,Read)

-- Deux blocs seront �gaux si et seulement si leurs conposants respectifs sont
-- en m�me nombre pr�sents dans les 2 blocs peu importe leur ordre et peu 
-- importe leur sitation sur un axe
instance Eq BLOC where
    Bloc bs1 == Bloc bs2 = 
        (foldr (&&) True (map (\bx -> elem bx bs2) bs1)) &&
                 (foldr (&&) True (map ((flip elem) bs1) bs2))

-- lesComposantsDuBloc bloc
-- retourne la liste des composants localis�s dans ce bloc
lesComposantsDuBloc (Bloc cs) = (map (snd.leCouple) cs)

-- faireBloc axe cs
-- cr�e un bloc � l'aide d'une liste de composants sur l'axe axe
faireBloc axe cs = Bloc (map (\c -> AxCmp (axe,c)) cs)

-- intersectionBlocs bloc1 bloc2
-- retourne le bloc intersection de bloc1 et de bloc2, les composants communs
-- aux 2 blocs se retrouvent dans l'intersection ind�pendemment de l'axe sur 
-- lequel ils se trouvent puisque l'�galit� au niveau des blocs n'utilise pas 
-- ce crit�re
intersectionBlocs (Bloc b1) (Bloc b2) = Bloc (intersect b1 b2)

-- unionBlocs bloc1 bloc2 
-- retourne un bloc form� des composants de bloc1 avec ceux de bloc2
unionBlocs (Bloc b1) (Bloc b2) = Bloc (union b1 b2)

-- dans un agencement, en principe, les composants de doivent pas avoir de 
-- position ind�termin�e
data AGENCEMENT = Agencement {lIdAgencement::String, lesComposants::[COMPOSANT]}
	          deriving (Eq,Show,Read,Ord)

estMeuble (Meuble _ _ _ _) = True
estMeuble _ = False

estElectro (Electro _ _ _ _) = True
estElectro _ = False

laPosition (Meuble _ _ (Position x y z) _) = Just (x, y, z)
laPosition (Electro _ _ (Position x y z) _) = Just (x, y, z)
laPosition _ = Nothing

laX composant = do
  (x,_,_) <- laPosition composant
  return x

laY composant = do
  (_,y,_) <- laPosition composant
  return y

laZ composant = do
  (_,_,z) <- laPosition composant
  return z

--laHauteur :: DIMENSION -> Float
laHauteur (h,_,_) = h

--laLargeur :: DIMENSION -> Float
laLargeur (_,l,_) = l

--laProfondeur :: DIMENSION -> Float
laProfondeur (_,_,p) = p

leComposant ag idc = find ((idc ==).lIdComposant) (lesComposants ag)

lesElectros ag = filter estElectro (lesComposants ag)

lesMeubles ag = filter estMeuble (lesComposants ag)
                                     
-- estVoisin' dir axe  c1 c2
-- retourne Maybe True si c1 est un voisin dans la direction dir de c2 sur l'axe
-- axe 
-- retourne Maybe False si c1 n'est pas un voisin dans la direction dir de c2 
-- sur l'axe axe
-- retourne Nothing si une des positions n'est pas d�finie
estVoisin' Droite axe c1 c2 = estVoisin' Gauche axe c2 c1
estVoisin' Gauche axe c1 c2 = do
  x1 <- laX c1
  x2 <- laX c2
  y1 <- laY c1
  y2 <- laY c2
  z1 <- laZ c1
  z2 <- laZ c2
  return (
          let
              l2 = (laLargeur . laDimension) c2
          in
            case axe of
              X -> x2 + l2 == x1 && y1 == y2 && z1 == z2
              otherwise -> y2 - l2 == y1 && x1 == x2 && z1 == z2)

-- estVoisin dir axe  c1 c2
-- retourne vrai si c1 est un voisin dans la direction dir de c2 sur l'axe axe
estVoisin dir axe c1 c2 = estVoisin' dir axe c1 c2 == Just True

-- leVoisin estVoisin ag compo
-- estVoisin est une fonction de voisinage
-- ag est un agencement
-- compo est un composant 
-- la fonction retourne peut-�tre (Maybe) l'�l�ment de l'agencement voisin
-- (au sens de estVoisin) du composant
leVoisin estVoisin ag compo = find (estVoisin compo) (lesComposants ag)

-- leBloc' estVoisin ag compo
-- estVoisin est une fonction de voisinage
-- ag est un agencement
-- compo est un composant
-- la fonction retourne la liste des composants qui sont voisins (au sens de la
-- fonction estVoisin) du composant
leBloc' estVoisin ag compo = 
    case leVoisin estVoisin ag compo of
      Nothing -> []
      Just voisin -> voisin : leBloc' estVoisin ag voisin

-- leBloc ax ag compo
-- ag est un agencement
-- compo est un composant
-- ax est un axe (X ou Y)
-- la fonction retourne un bloc form� des composants qui forment un bloc avec
-- compo sur l'axe ax 
leBloc ax ag compo = 
    faireBloc ax ((leBloc' (estVoisin Gauche ax) ag compo) ++ [compo] ++ 
                  (leBloc' (estVoisin Droite ax) ag compo))

sontDansMemeBloc ag c1 c2 = elem c2 (lesComposantsDuBloc (leBloc X ag c1)) || 
                            elem c2 (lesComposantsDuBloc (leBloc Y ag c1))

-- estDansBlocs blocs c 
-- retourne vrai si le composant c se trouve dans un des blocs de la liste de
-- blocs blocs
estDansBlocs blocs c = any (elem c) (map lesComposantsDuBloc blocs)

-- lesBloc' axe ag compos
-- axe est X ou Y
-- ag est un agencement
-- composant est une liste de composants
-- la fonction retourne la liste des blocs sur l'axe form�s avec chacun des 
-- composants de compos
lesBlocs' axe ag [] = []
lesBlocs' axe ag (compo:compos) =
    if estDansBlocs blocs compo then
        blocs
    else 
        (leBloc axe ag compo) : blocs
    where
      blocs = lesBlocs' axe ag compos

-- lesBloc axe ag
-- axe est X ou Y
-- ag est un agencement
-- la fonction retourne la liste des blocs sur l'axe X ou sur l'axe Y
lesBlocs axe ag = lesBlocs' axe ag (lesComposants ag)

-- lesComposantsDuType ag ty
-- ag est un agencement
-- ty est un type de composant
-- retourne la liste de des composants de l'agencement qui sont du type ty
--
-- Pour obtenir la liste demand�e, je part de la liste des composants de
-- l'agencement � l'aide de la fonction lesComposants. Ensuite, je filtre
-- pour include seulement les �l�ments de type ty, tout d'abord en obtenant
-- le type d'un composant � l'aide de leType, et en comparant le r�sultat
-- avec ty. On pourrait le faire aussi avec un filter.
lesComposantsDuType ag ty = [s | s<- (lesComposants ag), (leType s) == ty]

-- est vrai si et seulement si l'agencement ag poss�de tous les types de 
-- composants de la liste types
possedeTypes ag types = all ((0 < ) . length . (lesComposantsDuType ag)) types

-- retourne vrai si bloc n'est strictement inclus dans aucun des blocs de blocs
estDifferent blocs bloc = all (\ b ->  b == bloc || intersectionBlocs bloc b /= bloc) blocs

-- tous les blocs de blocs2 qui ne sont inclus dans aucun des blocs de blocs1
differentsTous blocs1 blocs2 = filter (estDifferent blocs1) blocs2

-- retourne tous les blocs de l'agencement ag sur les 2 axes X et Y.
-- la fonction �limine les blocs de l'axe Y qui sont inclus dans des
-- blocs de l'axe X et r�ciproquement
tousLesBlocs ag = differentsTous (lesBlocs X ag) (lesBlocs Y ag)  `union` 
                  differentsTous (lesBlocs Y ag) (lesBlocs X ag)

-- blocIsole blocs bloc
-- blocs est une liste de blocs
-- bloc est un bloc
-- blocIsole, cette fonction s'applique � une liste de blocs et � un bloc.
-- Elle retourne vrai si et seulement si,
-- soit le bloc n'a pas de composants en commun avec un des blocs de bloc,
-- soit il est inclus dans l'un d'entre eux.
--
-- blocIsole v�rifie les deux cas possibles d�crits. Tout d'abord on v�rifie
-- si un des composants de bloc est pr�sent dans un des blocs de bloc. Si
-- c'est le cas, on retourne faux. 
--
-- Si le premier cas retourne faux, on v�rifie alors le second cas. Pour ce
-- faire, on fait l'intersection de chaque element de blocs avec bloc. On
-- v�rifie ensuite si une des intersections est identique � bloc. En effet,
-- l'intersection de bloc avec un autre bloc ne peut contenir plus de 
-- composants que bloc. S'il y en a moins pr�sents, l'�galit� n'est pas
-- v�rifi�e.
blocIsole :: [BLOC] -> BLOC -> Bool
blocIsole blocs bloc =
    not (any (estDansBlocs blocs) (lesComposantsDuBloc bloc)) ||
    (any (==bloc) (map (intersectionBlocs bloc) blocs))

-- connecterUn blocs bloc
-- blocs est une liste de blocs
-- bloc est un bloc
-- retourne un bloc form� par la connection, si possible, de bloc � l'un des b
-- locs de blocs (deux blocs se connectent s'ils ont un composant en commun)
-- sinon retourne bloc lui-m�me
--
-- J'utilise ici la fonction d�finie composantCommun afin d'identifier � l'aide
-- de find le premier �l�ment de la liste blocs qui v�rifie composantCommun.
-- A ce moment, on peut cr�er le nouveau bloc. � l'aide du monad Maybe, on peut
-- identifier s'il y a deux blocs que l'on peut connecter. Si on peut le
-- faire, j'utilise unionBlocs pour obtenir le nouveau bloc � retourner, sinon
-- on retourne tout simplement bloc. Pour v�rifier si le r�sultat de find est
-- Nothing ou Just, j'utilise isNothing, et si le r�sultat n'est pas Nothing,
-- j'utilise fromJust pour obtenir le bloc d�sign�.
connecterUn :: [BLOC] -> BLOC -> BLOC
connecterUn blocs bloc = let b = find (composantCommun bloc) blocs in 
                       if isNothing b  then 
                           bloc
                       else
                           unionBlocs (fromJust b) bloc

-- composantCommun b1 b2
-- b1 et b2 sont deux blocs
-- retourne True ou False
-- Permet de v�rifier si deux blocs ont au moins un composant en commun. On
-- v�rifie en obtenant l'intersection des deux blocs et en v�rifiant si
-- le r�sultat de cette intersection est vide. Si elle est vide, il n'y a
-- donc pas de composant commun. 
composantCommun b1 b2 = not (null (lesComposantsDuBloc (intersectionBlocs b1 b2)))

-- retourne tous les blocs connect�s
-- si b1 est connect� � b2 alors b2 est aussi connect� � b1, cette connexion
-- figure en double on la supprime � l'aide de nub
connecterTous blocs = (nub . map (connecterUn blocs)) blocs

{-
tousLesBlocsConnectes ag =
    let
        blocs = connecterTous (tousLesBlocs ag)
    in
      differentsTous blocs blocs
-}


-- lesIds blocs
-- blocs est une liste de blocs
-- retourne la liste des listes des identificateurs des composants d'une liste 
-- de blocs
--
-- Pour obtenir la liste des liste de string, j'applique une composition de 
-- fonctions sur chaque element de la liste blocs. Tout d'abord, on obtient
-- les composants d'un bloc � l'aide de lesComposantsDuBloc, ensuite, on obtient
-- les identificateurs � l'aide de map lIdComposant. On applique cette composition
-- sur chaque bloc de la liste.
lesIds :: [BLOC] -> [[String]]
lesIds blocs = map ((map lIdComposant).lesComposantsDuBloc) blocs

