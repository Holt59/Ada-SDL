--------------------------------------------
--                                        --
--       PACKAGE GAME - PARTIE ADA        --
--                                        --
--            GAME-DISPLAY.ADS            --
--                                        --
--     Gestion de l'affichage principal   --
--                                        --
-- Créateur : CAPELLE Mikaël              --
-- Adresse  : capelle.mikael@gmail.com    --
--                                        --
-- Dernière modification : 14 / 06 / 2011 --
--                                        --
--------------------------------------------

with Game.Gtype;  use Game.Gtype;

package Game.Display is

   -- Lance le mode video, si une erreur survient, l'exception Video_Error est levée
   procedure Set_Video (Width            : in Size;              -- Largeur de la fenêtre
                        Height           : in Size;              -- Hauteur de la fenêtre
                        Depth            : in Profondeur := 32;  -- Profondeur (bits par pixel)
                        VRAM             : in Boolean := True;   -- VRAM ? (Mémoire Graphique => Plus rapide)
                        Full_Screen      : in Boolean := False;  -- Plein écran ?
                        Resizable        : in Boolean := False;  -- Redimensionnable ?
                        No_Frame         : in Boolean := False;  -- Sans bordure ?
                        Double_Buffering : in Boolean := True);  -- Double Buffer ? (Plus rapide)

   -- Retourne la surface principale
   function Get_Screen return Surface;

   -- Crée un rectangle
   -- Lève l'exception Surface_Error si une erreur survient
   function Create_Rectangle (Width     : in Size;             -- Largeur du rectangle
                              Height    : in Size;             -- Hauteur du rectangle
                              Depth     : in Profondeur := 32;   -- Profondeur (bits par pixel)
                              VRAM      : in Boolean := True)  -- VRAM ? (Mémoire Graphique => Plus rapide)
                             return Surface;

   -- Charge l'image ayant pour nom Name (ex : "image/mur.jpg"), retourne une surface avec un format correspondant au type d'image
   -- Lève l'exception Surface_Error si une erreur survient
   function Load_Image (Name : in String) return Surface;

   -- Sauvegarde une image au format BMP
   procedure Save_Image (Surf : in Surface;
                         Name : in String);

   -- Remplie la surface S avec la couleur C, si l'argument Re n'est pas spécifié, toutes la surface est remplie
   -- Lève l'exception Surface_Error si une erreur survient
   procedure Fill (Surf : in Surface;
                   Col  : in Color);

   procedure Fill (Surf   : in Surface;
                   Pos    : in Rect;
                   Col    : in Color);

   -- Colle la surface un morceau de la surface 'source' (définitie par la
   -- variable 'Partie' sur la surface Destination à la position 'Position
   -- Si 'Partie' n'est pas précisé, toute la surface Source est collé
   -- Si 'Position' n'est pas précisé, toute la surface Sourcé est collé
   -- en haut à gauche de 'Destination
   -- Lève l'exception Surface_Error en cas d'échec
   procedure Blit (Destination  : in out Surface;
                   Source       : in     Surface);

   procedure Blit (Destination  : in out Surface;
                   Source       : in     Surface;
                   Position     : in     Rect);

   procedure Blit (Destination  : in out Surface;
                   Source       : in     Surface;
                   Position     : in     Rect;
                   Partie       : in     Rect);

   -- Mais à jour les informations de l'écran
   -- !!! Sans appel à Flip, toutes les informations restent stocké en mémoire
   -- et aucun changement n'apparait à l'écran !!!
   procedure Flip;

   -- Retourne la surface S avec le même format que la surface principale
   -- Posséder 2 surface avec le même format améliore le blit entre les 2
   -- Cette fonction détruit la transparence
   function Display_Format (Surf  : in Surface;
                            Alpha : in Boolean := False)
                           return Surface;

   -- Transforme tous les pixels de la couleur C de la surface S en pixel
   -- transparent si Active = ENABLE, annule l'effet sinon

   procedure Set_Color_Key (Surf : in Surface;
                            Col  : in Color;
                            Flag : in Active := ENABLE);

   -- Montre ou cache le curseur de la souris
   procedure Show_Cursor(Flag : in Active := ENABLE);

end Game.Display;
