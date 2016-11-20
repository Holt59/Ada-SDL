--------------------------------------------
--                                        --
--       PACKAGE GAME - PARTIE ADA        --
--                                        --
--          GAME-DISPLAY-DRAW.ADB         --
--                                        --
--      Gestion du dessin sur surface     --
--                                        --
-- Créateur : CAPELLE Mikaël              --
-- Adresse  : capelle.mikael@gmail.com    --
--                                        --
-- Dernière modification : 14 / 06 / 2011 --
--                                        --
--------------------------------------------

with Ada.Exceptions;
with Game.Gtype, Game, Game.Display, Interfaces.C;
use Game.Gtype, Game, Interfaces.C;

package body Game.Display.Draw is

   ---------------
   -- PUT_PIXEL --
   ---------------

   procedure C_Put_Pixel(S : in SDL_Surface; X,Y : in Int; C : in out C_Color);
   pragma Import(C,C_Put_Pixel,"put_pixel");

   procedure Put_Pixel (Surf : in out Surface;
                        X,Y  : in     Coord;
                        Col  : in     Color) is
      CC : C_Color := To_C_Col(Col);
   begin
      if X < 0 or X >= Get_Width(Surf) or Y < 0 or Y >= Get_Height(Surf) then
         Ada.Exceptions.Raise_Exception(Draw_Error'Identity, "Pixel hors surface.");
      else
         C_Put_Pixel(Surf.Surf,Int(X),Int(Y),CC);
      end if;
   end Put_Pixel;

   ---------------
   -- GET_PIXEL --
   ---------------

   function C_Get_Pixel(S : in SDL_Surface; X,Y : in Int) return C_Color;
   pragma Import(C,C_Get_Pixel,"get_pixel");

   function Get_Pixel (Surf : in Surface;
                       X,Y  : in Coord) return Color is
      C : C_Color;
   begin
      if X < 0 or X >= Get_Width(Surf) or Y < 0 or Y >= Get_Height(Surf) then
         Ada.Exceptions.Raise_Exception(Draw_Error'Identity, "Pixel hors surface.");
         return (0,0,0); -- Ne devrait jamais être atteint
      else
         C := C_Get_Pixel(Surf.Surf,Int(X),Int(Y));
         return (Integer(C.R),Integer(C.G),Integer(C.B));
      end if;
   end Get_Pixel;

   ----------------------
   -- (UN)LOCK_SURFACE --
   ----------------------

   procedure C_Lock(S : in SDL_Surface);
   pragma Import(C,C_Lock,"lock_surface");
   procedure Lock_Surface(Surf : in Surface) is
   begin
      C_Lock(Surf.Surf);
   end Lock_Surface;

   procedure C_Unlock(S : in SDL_Surface);
   pragma Import(C,C_Unlock,"unlock_surface");
   procedure Unlock_Surface(Surf : in Surface) is
   begin
      C_Unlock(Surf.Surf);
   end Unlock_Surface;

   -------------------------------
   -- PROCEDURE ADA SANS IMPORT --
   -------------------------------

   -- procedure qui tente un put_pixel et ne fais rien en cas d'erreur

   procedure NoExcept_Put_Pixel(S : in out Surface; X,Y : in Integer; C : in Color) is
   begin
      Put_Pixel(S,X,Y,C);
   exception
      when Draw_Error =>
         null ;
   end NoExcept_Put_Pixel;

   ------------
   -- CERCLE --
   ------------

   procedure Cercle(Surf      : in out Surface;
                    Centre    : in     Rect;
                    Col       : in     Color;
                    Rayon     : in     Positive;
                    Epaisseur : in     Positive := 1;
                    Lock      : in     Boolean := True) is

      WS : constant Size := Get_Width(Surf);
      HS : constant Size := Get_Height(Surf);

      procedure Tracer_Pixel(X,Y : in Coord) is
      begin
         NoExcept_Put_Pixel(Surf,Centre.X+X,Centre.Y+Y,Col);
         NoExcept_Put_Pixel(Surf,Centre.X+X,Centre.Y-Y,Col);
         NoExcept_Put_Pixel(Surf,Centre.X-X,Centre.Y+Y,Col);
         NoExcept_Put_Pixel(Surf,Centre.X-X,Centre.Y-Y,Col);
         NoExcept_Put_Pixel(Surf,Centre.X+Y,Centre.Y+X,Col);
         NoExcept_Put_Pixel(Surf,Centre.X+Y,Centre.Y-X,Col);
         NoExcept_Put_Pixel(Surf,Centre.X-Y,Centre.Y+X,Col);
         NoExcept_Put_Pixel(Surf,Centre.X-Y,Centre.Y-X,Col);
      end Tracer_Pixel;

      X : Coord := 0;
      Y : Coord := Rayon;
      D : Coord := Rayon - 1;
      Min : Coord := -(Epaisseur-1)/2;
      Max : Coord := Epaisseur/2;
   begin
      if Lock then
         Lock_Surface(Surf);
      end if;
      for N in Min..Max loop
         X := 0;
         Y := Rayon + N;
         D := (Rayon + N) - 1;
         while X <= Y loop
            Tracer_Pixel(X,Y);
            if D >= 2*X then
               D := D - 2*X - 1;
               X := X + 1;
            elsif D <= 2*((Rayon+N)-Y) then
               D := D + 2*Y - 1;
               Y := Y - 1;
            else
               D := D + 2*(Y-X-1);
               X := X + 1;
               Y := Y - 1;
            end if;
         end loop;
      end loop;
      if Lock then
         Unlock_Surface(Surf);
      end if;
   end Cercle;

   ------------
   -- DISQUE --
   ------------

   procedure Disque(Surf      : in out Surface;
                    Centre    : in     Rect;
                    Col       : in     Color;
                    Rayon     : in     Positive;
                    Lock      : in Boolean := True) is
      Sqrt2   : constant Float := 1.4;
      Lg_Bloc : constant Natural := Natural(Sqrt2/2.0*Float(Rayon));
      Bloc    : constant Rect := (Centre.X - Lg_Bloc,
                                  Centre.Y - Lg_Bloc,
                                  Natural(Float(Rayon)*Sqrt2),
                                  Natural(Float(Rayon)*Sqrt2));
   begin
      Game.Display.Fill(Surf,Bloc,Col);
      if Lock then
         Lock_Surface(Surf);
      end if;
      for N in Lg_Bloc .. Rayon loop
         Cercle(Surf,Centre,Col,N,1,False);
      end loop;
      if Lock then
         Unlock_Surface(Surf);
      end if;
   end Disque;

   procedure Segment_Bresenham (S : in out Surface; X1,Y1 : in Integer; X2,Y2 : in Integer; Col : in Color; Lock : in Boolean := True) is
      X : Integer := X1;
      Y : Integer := Y1;
      Coeff : Float := 0.0;
      First, Last : Integer := 0;
      Sign : Integer := 1;
   begin
      if Lock then
         Lock_Surface(S);
      end if;

      if X1 = X2 then
         if Y1 < Y2 then
            First := Y1;
            Last := Y2;
         else
            First := Y2;
            Last := Y1;
         end if;
         for Y in First..Last loop
            Put_Pixel(S,X1,Y,Col);
         end loop;
      else
         if X2 - X1 < 0 then
            Sign := -1;
         end if;
         Coeff := Float(Sign)*Float(Y2-Y1)/Float(X2-X1);
         if Coeff <= 1.0 and Coeff >= -1.0 then
            if Sign = 1 then
               for X in X1..X2 loop
                  Y := Y1 + Integer(Coeff*Float(X-X1));
                  Put_Pixel(S,X,Y,Col);
               end loop;
            else
               for X in X2..X1 loop
                  Y := Y1 + Integer(Coeff*Float(X1-X));
                  Put_Pixel(S,X,Y,Col);
               end loop;
            end if;
         else
            Coeff := Float(X2-X1)/Float(Y2-Y1);
            Last := abs (Y2 - Y1);
            for Y in 0..Last loop
               X := X1 + Sign * (abs Integer(Coeff*Float(Y)));
               if Y1 <= Y2 then
                  Put_Pixel(S,X,Y1+Y,Col);
               else
                  Put_Pixel(S,X,Y1-Y,Col);
               end if;
            end loop;
         end if;
      end if;
      if Lock then
         Unlock_Surface(S);
      end if;
   exception
      when others => null;
   end Segment_Bresenham;

   procedure Segment(Surf     : in out Surface;
                     X1,Y1    : in     Integer;
                     X2,Y2    : in     Integer;
                     Col      : in     Color;
                     Lock     : in     Boolean := True) is
   begin
      Segment_Bresenham(Surf,X1,Y1,X2,Y2,Col,Lock);
   end Segment;

end Game.Display.Draw;
