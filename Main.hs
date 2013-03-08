-- *********************************************************************
-- * TP1
-- Scenario de tests
--
-- Modifié par Guillaume Lahaie
-- LAHG04077707
-- Dernière modification: 7 mars 2013
--
-- Ajout de tests pour compléter les essais des fonctions définies
-- *********************************************************************/
module Main where

-- --------------------------------------------------------------
-- Chargement des modules nécessaires au scénario de tests
-- --------------------------------------------------------------
import Agencement

b1 = Meuble "b1" (1.0,1.0,1.0) (Position 0.0 0.0 0.0) Coin
b2 = Electro "b2" (1.0,1.0,1.0) (Position 1.0 0.0 0.0) LaveVaisselle
b3 = Meuble "b3" (1.0,1.0,1.0) (Position 0.0 1.0 0.0) Bas
b4 = Meuble "b4" (1.0,1.0,1.0) (Position 2.0 0.0 0.0) Bas
h1 = Meuble "h1" (1.0,1.0,0.5) (Position 2.0 0.0 2.0) Haut
h2 = Meuble "h2" (1.0,1.0,0.5) (Position 3.0 0.0 2.0) Haut
b5 = Meuble "b5" (1.0,1.0,1.0) (Position 4.0 0.0 0.0) Haut
b6 = Meuble "b6" (1.0,1.0,1.0) (Position 5.0 0.0 0.0) Haut

bloc = Bloc [AxCmp (Y,b1),AxCmp (Y,b3)]

bloc1 = Bloc [AxCmp (X,b1),AxCmp (X,b2),AxCmp (X,b4)]

bloc2 = Bloc [AxCmp (X,b5),AxCmp (X,b6)]

bloc3 = Bloc [AxCmp (X,h1),AxCmp (X,h2)]

bloc1P = Bloc [AxCmp (X,b1),AxCmp (X,b4),AxCmp (Y,b2)]

bloc1PP = Bloc [AxCmp (X,b1),AxCmp (X,b2)]

blocUn = Bloc [AxCmp (Y,b1)]

blocVide1 = Bloc [AxCmp (Y, Meuble "" (0.0, 0.0, 0.0) (Position 2.0 0.0 2.0) Haut)]

bloc1EtBloc = Bloc [AxCmp (X,b1),AxCmp (X,b2),AxCmp (Y,b3),AxCmp (X,b4)]

blocs = [bloc1, bloc2, bloc3]

ag = Agencement "ag" [b1,b2,b3,b4,b5,b6,h1,h2] 

agVide = Agencement "vide" []

agUn = Agencement "un" [h1]

agDeux = Agencement "deux" [b1,b3]


testXX m p = if p then do
		       putStr ("\n" ++ m ++ ": ")
		       putStr "OK\n"
		       return 1 
	     else do
		  putStr ("\n" ++ m ++ ": ")
		  putStr "ERREUR\n"
		  return 0

-- Description des tests
-- ----------------------------------------------------------------

trXX 1 = testXX "#1. lesIds: bloc vide"
	 (lesIds [] == [])
trXX 2 = testXX "#2. lesIds: liste d'un bloc avec un composant"
	 (lesIds [blocUn] == [["b1"]])
trXX 3 = testXX "#3. lesIds: liste d'un bloc avec plusieurs composants"
	 (lesIds [bloc] == [["b1","b3"]])
trXX 4 = testXX "#4. lesIds: liste de plusieurs blocs"
	 (lesIds blocs == [["b1","b2","b4"],["b5","b6"],["h1","h2"]])
trXX 5 = testXX "#5. lesComposantsDuType : agencement vide, rien"
         (map lIdComposant (lesComposantsDuType agVide Four) == [])
trXX 6 = testXX "#6. lesComposantsDuType : agencement un composant, rien"
         (map lIdComposant (lesComposantsDuType agUn Four) == [])
trXX 7 = testXX "#7. lesComposantsDuType : agencement un composant, un composant"
         (map lIdComposant (lesComposantsDuType agUn Haut) == ["h1"])
trXX 8 = testXX "#8. lesComposantsDuType : agencement 2 composants, un composant"
         (map lIdComposant (lesComposantsDuType agDeux Coin) == ["b1"])
trXX 9 = testXX "#9. lesComposantsDuType : agencement plusieurs composants,\n un composant"
         (map lIdComposant (lesComposantsDuType ag Coin) == ["b1"])
trXX 10 = testXX "#10. lesComposantsDuType : agencement plusieurs composants,\n plusieurs composants"
         (map lIdComposant (lesComposantsDuType ag Bas) == ["b3","b4"])
trXX 11 = testXX "#11. blocIsole : liste de blocs vide"
         (blocIsole [] bloc1)
trXX 12 = testXX "#12. blocIsole : liste avec un autre bloc"
         ((not . blocIsole [bloc]) bloc1P)
trXX 13 = testXX "#13. blocIsole : liste avec un même bloc"
         (blocIsole [bloc1] bloc1P)
trXX 14 = testXX "#14. blocIsole : liste avec des blocs dont un qui inclut"
         (blocIsole blocs bloc1PP)
trXX 15 = testXX "#15. blocIsole : liste avec des blocs dont aucun n'inclut"
         ((not . blocIsole blocs) bloc)
trXX 16 = testXX "#16. connecterUn : liste de blocs vide"
	 (connecterUn [] bloc == bloc)
trXX 17 = testXX "#17. connecterUn : liste de 1 bloc avec bloc non connectable"
	 (connecterUn [bloc1] bloc2 == bloc2)
trXX 18 = testXX "#18. connecterUn : liste de 1 bloc avec bloc connectable"
	 (connecterUn [bloc1] bloc == bloc1EtBloc)
trXX 19 = testXX "#19. connecterUn : liste de blocs avec un bloc non connectable"
	 (connecterUn [bloc1,bloc2] bloc3 == bloc3)
trXX 20 = testXX "#20. connecterUn : liste de blocs avec un bloc connectable"
	 (connecterUn blocs bloc == bloc1EtBloc)
trXX 21 = testXX "#21. lesIds : Test sur une liste de blocs ou id est vide"
	  (lesIds [blocVide1, blocVide1] == [[""],[""]])
trXX 22 = testXX "#22. lesComposantsDuType : agencement plusieurs composants,\naucun composant"
         (map lIdComposant (lesComposantsDuType ag Four) == [])
trXX 23 = testXX "#23. blocIsole : liste avec des blocs dont plusieurs incluent"
         (blocIsole [bloc1, bloc1] bloc1)
trXX 24 = testXX "#24. composantCommun : deux blocs avec composant commun"
         (composantCommun bloc bloc1)
trXX 25 = testXX "#25. composantCommun : deux blocs sans composant commun"
         (not (composantCommun bloc bloc2))
                  
         
-- Impression et calcul du résultat
-- ----------------------------------------------------------------

main = do
  putStr "\n\nTESTS POUR -- lesIds(...)\n==================================================================\n"
  n1 <- trXX 1
  n2 <- trXX 2 
  n3 <- trXX 3
  n4 <- trXX 4
  putStr ("\nNOTE POUR CETTE PARTIE: " ++ show ((n1+n2)*0.5+n3+n4) ++ "/3\n\n")
  putStr "\n\nTESTS POUR -- lesComposantsDuType(...)\n==================================================================\n"
  n5 <- trXX 5
  n6 <- trXX 6
  n7 <- trXX 7
  n8 <- trXX 8
  n9 <- trXX 9
  n10 <- trXX 10
  putStr ("\nNOTE POUR CETTE PARTIE: " ++ show ((n5+n6)*0.5+n7+n8+n9+n10) ++ "/5\n")
  putStr "\n\nTESTS POUR blocIsole\n==================================================================\n"
  n11 <- trXX 11
  n12 <- trXX 12
  n13 <- trXX 13
  n14 <- trXX 14
  n15 <- trXX 15
  putStr ("\nNOTE POUR CETTE PARTIE: " ++ show ((n11+n12)*0.5+n13+n14+n15) ++ "/4\n")
  putStr "\n\nTESTS POUR connecterUn\n==================================================================\n"
  n16 <- trXX 16
  n17 <- trXX 17
  n18 <- trXX 18
  n19 <- trXX 19
  n20 <- trXX 20
  putStr ("\nNOTE POUR CETTE PARTIE: " ++ show ((n16+n17)*0.5+n18+n19+n20) ++ "/4\n")
  putStr ("\n\nNOTE FINALE: " ++ show ((n1+n2)*0.5+n3+n4+(n5+n6)*0.5+n7+n8+n9+n10+(n11+n12)*0.5+n13+n14+n15+(n16+n17)*0.5+n18+n19+n20) ++ "/16\n")
  putStr "\n\nAUTRES TESTS \n==================================================================\n"
  trXX 21
  trXX 22
  trXX 23
  trXX 24
  trXX 25
  putStr "\n"

