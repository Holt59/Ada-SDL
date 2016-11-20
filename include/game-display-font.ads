--------------------------------------------
--                                        --
--       PACKAGE GAME - PARTIE ADA        --
--                                        --
--         GAME-DISPLAY-FONT.ADS          --
--                                        --
--     Gestion de l'affichage du texte    --
--                                        --
-- Créateur : CAPELLE Mikaël              --
-- Adresse  : capelle.mikael@gmail.com    --
--                                        --
-- Dernière modification : 14 / 06 / 2011 --
--                                        --
--------------------------------------------

with Interfaces.C, Game.Gtype;
use Interfaces.C, Game.Gtype;

with Ada.Finalization;

package Game.Display.Font is

   -- Type stockant les polices
   type Font is limited private;

   -- Type gérant l'encodage
   -- Je ne détaillerais pas ici les règles d'encodage
   -- Sachez juste que UTF8 gère un plus grand nombre de caractère
   -- Faites des tests, mais il sera très rare d'utilisé LATIN1
   type Encodage is (LATIN1, UTF8);

   -- Type gérant le rendu
   -- SOLID   : Le texte sera assez brut (pas de lissage), aura un fond
   --           transparent, et la création sera très rapide (idéal pour des
   --           compteurs, etc... )
   -- SHADED  : Le texte sera lissé mais devra avoir un fond unie (définie par
   --           fond), création très rapide aussi
   -- BLENDED : Le texte sera lissé sur un fond transparent, en revanche la
   --           création sera lente
   type Type_Rendu is (SOLID,SHADED,BLENDED);

   -- Ouvre / Ferme une police
   -- Utilisez une police déjà ouverte dans les fonctions d'affichage
   -- de texte accélère leur fonctionnement
   -- Lève l'exception Font_Error si une erreur survient
   procedure  Open_Font  (F      : out Font;
                          Nom    : in String;
                          Taille : in Integer);

   -- Retourne une surface contenant le texte fournit en argument avec le
   -- format demandé
   -- Lève l'exception Font_Error si une erreur survient
   function Get_Text_Surf (Text      : in String;                       -- Texte à afficher
                           Police    : in Font;                         -- Police (sous le format d'un type Font)
                           F_Type    : in Type_Rendu := BLENDED;        -- Type de rendu (cf. au dessus)
                           Bold      : in Boolean    := False;          -- Gras (True)
                           Underline : in Boolean    := False;          -- Souligné (True)
                           Italic    : in Boolean    := False;          -- Italic (True)
                           Encode    : in Encodage   := UTF8;           -- Encodage (cf. au dessus)
                           Coul      : in Color      := (0,0,0);        -- Couleur du texte
                           Fond      : in Color      := (255,255,255))  -- Couleur de fond
                          return Surface;

   function Get_Text_Surf(Text      : in String;                        -- Texte à afficher
                          Police    : in String;                        -- Nom de la police (fichier .ttf)
                          Taille    : in Natural;                       -- Taille de la police
                          F_Type    : in Type_Rendu := BLENDED;         -- Type de rendu (cf. au dessus)
                          Bold      : in Boolean    := False;           -- Gras (True)
                          Underline : in Boolean    := False;           -- Souligné (True)
                          Italic    : in Boolean    := False;           -- Italic (True)
                          Encode    : in Encodage   := UTF8;            -- Encodage (cf. au dessus)
                          Coul      : in Color      := (0,0,0);         -- Couleur du texte
                          Fond      : in Color      := (255,255,255))   -- Couleur de fond
                         return Surface;

   -- Ecrit le texte passé en argument à la position demandé sur la surface
   -- Surf avec le formatage adéquat
   -- Lève l'exception Font_Error si une erreur survient
   procedure Print_Text(Surf      : in out Surface;                  -- Surface sur laquelle affiché le texte
                        Pos       : in out Rect;                     -- Position d'affichage du texte (sert aussi à stocker la taille du bloc pris par le texte)
                        Text      : in String;                       -- Texte à afficher
                        Police    : in Font;                         -- Police (sous le format d'un type Font)
                        F_Type    : in Type_Rendu := BLENDED;        -- Type de rendu (cf. au dessus)
                        Bold      : in Boolean    := False;          -- Gras (True)
                        Underline : in Boolean    := False;          -- Souligné (True)
                        Italic    : in Boolean    := False;          -- Italic (True)
                        Encode    : in Encodage   := UTF8;           -- Encodage (cf. au dessus)
                        Coul      : in Color      := (0,0,0);        -- Couleur du texte
                        Fond      : in Color      := (255,255,255)); -- Couleur de fond


   procedure Print_Text(Surf      : in out Surface;                  -- Surface sur laquelle affiché le texte
                        Pos       : in out Rect;                     -- Position d'affichage du texte (sert aussi à stocker la taille du bloc pris par le texte)
                        Text      : in String;                       -- Texte à afficher
                        Police    : in String;                       -- Nom de la police (fichier .ttf)
                        Taille    : in Natural;                      -- Taille de la police
                        F_Type    : in Type_Rendu := BLENDED;        -- Type de rendu (cf. au dessus)
                        Bold      : in Boolean    := False;          -- Gras (True)
                        Underline : in Boolean    := False;          -- Souligné (True)
                        Italic    : in Boolean    := False;          -- Italic (True)
                        Encode    : in Encodage   := UTF8;           -- Encodage (cf. au dessus)
                        Coul      : in Color      := (0,0,0);        -- Couleur du texte
                        Fond      : in Color      := (255,255,255)); -- Couleur de fond

private

   package AF renames Ada.Finalization;

   type SDL_Font is access all Interfaces.C.Int;

   Null_SDL_Font : constant SDL_Font := null;

   procedure Close_Font (F : in out Font);

   type Font is new AF.Limited_Controlled with
      record
         Fon : SDL_Font;
      end record;

   procedure Initialize (F : in out Font);
   procedure Finalize   (F : in out Font);

end Game.Display.Font;
