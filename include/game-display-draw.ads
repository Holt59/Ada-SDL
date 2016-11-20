--------------------------------------------
--                                        --
--       PACKAGE GAME - PARTIE ADA        --
--                                        --
--          GAME-DISPLAY-DRAW.ADS         --
--                                        --
--      Gestion du dessin sur surface     --
--                                        --
-- Créateur : CAPELLE Mikaël              --
-- Adresse  : capelle.mikael@gmail.com    --
--                                        --
-- Dernière modification : 14 / 06 / 2011 --
--                                        --
--------------------------------------------

with Game.Gtype, Game;
use Game.Gtype, Game;

package Game.Display.Draw is

   -- Pour utiliser Put_Pixel et Get_Pixel, la surface S doit être bloqué (lock)

   -- Transforme le Pixel contenue à la position (X,Y) de la surface S
   procedure Put_Pixel (Surf : in out Surface;
                        X,Y  : in     Coord;
                        Col  : in     Color);

   -- Récupère la couleur du Pixel se trouvant à la position (X,Y) de la
   -- surface S
   function Get_Pixel (Surf : in Surface;
                       X,Y  : in Coord) return Color;

   -- Une surface doit être bloqué avant tout dessin dessus

   -- Bloque (Lock) ou Debloque (Unlock) la surface S
   procedure Lock_Surface   (Surf : in Surface);
   procedure Unlock_Surface (Surf : in Surface);

   -- Dessine un cercle
   procedure Cercle(Surf      : in out Surface;           -- Surface sur laquelle le cercle sera dessiné
                    Centre    : in     Rect;              -- Position du centre du cercle
                    Col       : in     Color;             -- Couleur du cercle
                    Rayon     : in     Positive;          -- Rayon du cercle
                    Epaisseur : in     Positive := 1;     -- Epaisseur du cercle (bord)
                    Lock      : in     Boolean := True);  -- Surface à bloquer avant le dessin ?

   -- Dessine un disque
   procedure Disque(Surf      : in out Surface;           -- Surface sur laquelle le disque sera dessiné
                    Centre    : in     Rect;              -- Position du centre du disque
                    Col       : in     Color;             -- Couleur du disque
                    Rayon     : in     Positive;          -- Rayon du disque
                    Lock      : in Boolean := True);      -- Surface à bloquer avant le dessin ?

   -- Trace un segmen
   procedure Segment(Surf     : in out Surface;           -- Surface sur laquelle le segment sera tracé
                     X1,Y1    : in     Integer;           -- Position du point de départ
                     X2,Y2    : in     Integer;           -- Position du point d'arrivée
                     Col      : in     Color;             -- Couleur du segment
                     Lock     : in     Boolean := True);  -- Surface à bloquer avant le tracé ?

end Game.Display.Draw;
