--------------------------------------------
--                                        --
--       PACKAGE GAME - PARTIE ADA        --
--                                        --
--             GAME-GTYPE.ADB             --
--                                        --
--            Gestion des types           --
--                                        --
-- Créateur : CAPELLE Mikaël              --
-- Adresse  : capelle.mikael@gmail.com    --
--                                        --
-- Dernière modification : 14 / 06 / 2011 --
--                                        --
--------------------------------------------

with Ada.Characters.Handling;


package body Game.Gtype is

   package Test_Char renames Ada.Characters.Handling;

   ---------------
   -- "+" COLOR --
   ---------------

   -- Retourne Vrai si la chaine est bien une couleur en hexa (de la forme "0xaabbcc")
   function Is_Hexa (S : in String) return Boolean is
      Res : Boolean := True;
   begin
      -- Vérification de la taille et du '0x' de départ
      if S'Length /= 8 or S(S'First) /= '0' or S(S'First+1) /= 'x' then
         Res := False;
      else
         -- Vérification de chaque caractère (aabbcc)
         for N in S'First+2..S'Last loop
            if not Test_Char.Is_Hexadecimal_Digit(S(N)) then
               Res := False;
            end if;
         end loop;
      end if;
      return Res;
   end Is_Hexa;

   -- Retourne la valeur hexadécimal d'un charactère ('1' => 1, 'A' => 10)
   function Hexa_Val (C : in Character) return Natural is
      Res : Natural := 0;
   begin
      -- Si C est un chiffre, on retourne "Position(C) - Position(0)"
      -- Si C est un minuscule (resp. une majuscule), on retourne "10 + Position(C) - Position('a') (resp. Position('A'))
      if Test_Char.Is_Digit(C) then
         Res := Character'Pos(C) - Character'Pos('0');
      elsif Test_Char.Is_Lower(C) then
         Res := 10 + Character'Pos(C) - Character'Pos('a');
      elsif Test_Char.Is_Upper(C) then
         Res := 10 + Character'Pos(C) - Character'Pos('A');
      else
         raise Constraint_Error; -- Ne devrait jamais arrivé
      end if;
      return Res;
   end Hexa_Val;

   -- Transforme une couleur sous la forme hexa en une "color"
   function "+" (S : in String) return Color is
      Res : Color := (0,0,0);
   begin
      if not Is_Hexa(S) then
         raise Constraint_Error;
      end if;
      Res.R := Hexa_Val(S(S'First+2))*16 + Hexa_Val(S(S'First+3));
      Res.G := Hexa_Val(S(S'First+4))*16 + Hexa_Val(S(S'First+5));
      Res.B := Hexa_Val(S(S'First+6))*16 + Hexa_Val(S(S'First+7));
      return Res;
   end "+";

   -----------
   -- GET_X --
   -----------

   -- Simple appel de fonction C transformé en des types plus "ada"

   function C_Getw(S : in SDL_Surface) return integer;
   pragma Import(C,C_Getw,"get_width");
   function C_Geth(S : in SDL_Surface) return Integer;
   pragma Import(C,C_Geth,"get_height");
   function C_Getd(S : in SDL_Surface) return Integer;
   pragma Import(C,C_Getd,"get_depth");

   function Get_Width (S : in Surface) return Size is
   begin
      return Size(C_Getw(S.Surf));
   end Get_Width;

   function Get_Height (S : in Surface) return Size is
   begin
      return Size(C_Geth(S.Surf));
   end Get_Height;

   function Get_Rect (S : in Surface) return Rect is
   begin
      return (0,0,Get_Width(S),Get_Height(S));
   end Get_Rect;

   function Get_Depth (S : in Surface) return Profondeur is
   begin
      return Profondeur(C_Getd(S.Surf));
   end Get_Depth;

   ---------------
   -- TO_C_RECT --
   ---------------

   -- Transforme un Rect (Ada) en Rect (C)
   function To_C_Rect(Re : in Rect) return C_Rect is
      Res : C_Rect := (0,0,0,0);
   begin
      -- On transforme chaque composante comme il faut
      Res.X := Signed_16(Re.X);
      Res.Y := Signed_16(Re.Y);
      Res.W := Unsigned_16(Re.W);
      Res.H := Unsigned_16(Re.H);
      return Res;
   end To_C_Rect;

   -- Transforme une couleur (Ada) en couleur (C)
   function To_C_Col (C : in Color) return C_Color is
   begin
      return (Unsigned_8(C.R),Unsigned_8(C.G),Unsigned_8(C.B),0);
   end To_C_Col;

end Game.Gtype;
