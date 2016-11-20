--------------------------------------------
--                                        --
--       PACKAGE GAME - PARTIE ADA        --
--                                        --
--             GAME-GTYPE.ADS             --
--                                        --
--            Gestion des types           --
--                                        --
-- Créateur : CAPELLE Mikaël              --
-- Adresse  : capelle.mikael@gmail.com    --
--                                        --
-- Dernière modification : 14 / 06 / 2011 --
--                                        --
--------------------------------------------

with Interfaces.C, Interfaces.C.Extensions;
use Interfaces.C, Interfaces.C.Extensions;

package Game.gtype is

   -- Type utilisé pour l'activation ou la désactivation de fonction spécifique
   type Active is (ENABLE,DISABLE);

   Max_Profondeur : constant := 32;
   type Profondeur is range 0 .. Max_Profondeur ;

   Max_Size : constant Natural := 2**16 - 1;
   subtype Size is Natural range 0 .. Max_Size;

   Max_Coordonnee : constant Integer := 2**15 - 1;
   subtype Coord is Integer range (- Max_Coordonnee + 1) .. Max_Coordonnee;

   -- Type servant à définir un rectangle
   type Rect is
      record
         X,Y : Coord := 0;      -- Point haut gauche du rectangle
         W,H : Size  := 0;      -- Largeur et Hauteur
      end record;

   -- Les fonctions suivantes retournent respectivement la largeur, la
   -- hauteur et la profondeur d'une Surface
   function Get_Width  (S : in Surface) return Size;
   function Get_Height (S : in Surface) return Size;
   function Get_Depth  (S : in Surface) return Profondeur;

   -- Retourne un Rect contenant la largeur et la hauteur de la Surface,
   -- les coordonnées X,Y du résultat sont mis (0,0)
   function Get_Rect(S : in Surface) return Rect;

   -- Type pour créer des couleurs à partir des composantes rouge,
   -- verte et bleu
   type Color is
      record
         R : Integer range 0..255;
         G : Integer range 0..255;
         B : Integer range 0..255;
      end record;

   -- Retourne la valeur d'une couleur à partir de son code hexadécimal
   -- Le code doit commencer par 0x
   -- Ex : +"0xff00ff" retourne (255,0,255)
   --      +"0x000000" retourne (0,0,0)
   -- L'utilisation d'un mauvais code hexadécimal lève l'exception Constraint_Error
   function "+" (S : in String) return Color;

   -- Valeur de différentes couleurs basique
   Blanc  : constant Color := (255,255,255);
   Noir   : constant Color := (  0,  0,  0);
   Rouge  : constant Color := (255,  0,  0);
   Bleu   : constant Color := (  0,  0,255);
   Violet : constant Color := (255,  0,255);
   Jaune  : constant Color := (255,255,  0);
   Vert   : constant Color := (  0,255,  0);
   Cyan   : constant Color := (  0,255,255);










------------------------------------------------------------------------------------
---------------------- Partie INUTILE pour l'utilisateur ---------------------------
------------------------------------------------------------------------------------

   type C_Rect is
      record
         X,Y : Signed_16;
         W,H : Unsigned_16;
      end record;
   pragma Convention(C,C_Rect);
   function To_C_Rect (Re : in Rect) return C_Rect;

   type C_Color is
      record
         R,G,B,U : Unsigned_8;
      end record;
   pragma Convention(C,C_Color);
   function To_C_Col (C : in Color) return C_Color;

end Game.gtype;
