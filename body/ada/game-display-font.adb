--------------------------------------------
--                                        --
--       PACKAGE GAME - PARTIE ADA        --
--                                        --
--         GAME-DISPLAY-FONT.ADB          --
--                                        --
--     Gestion de l'affichage du texte    --
--                                        --
-- Créateur : CAPELLE Mikaël              --
-- Adresse  : capelle.mikael@gmail.com    --
--                                        --
-- Dernière modification : 14 / 06 / 2011 --
--                                        --
--------------------------------------------

with Interfaces.C, Interfaces.C.Extensions, Game.Display, Game.Gtype, Ada.Exceptions;
use Interfaces.C, Interfaces.C.Extensions, Game.Display, Game.Gtype;

with Ada.Text_IO;

package body Game.Display.Font is

   -----------------
   -- CLOSE_FONT  --
   -----------------

   procedure C_Close_Font(F : in SDL_Font);
   pragma Import(C,C_Close_Font,"close_font");

   procedure Close_Font (F : in out Font) is
   begin
      if F.Fon /= Null_SDL_Font then
         C_Close_Font(F.Fon);
         F.Fon := Null_SDL_Font;
      end if;
   end Close_Font;


   ---------------
   -- OPEN_FONT --
   ---------------

   function C_Open_Font(Nom : in Char_Array; Taille : in Int) return SDL_Font;
   pragma Import(C,C_Open_Font,"open_font");

   procedure  Open_Font  (F      : out Font;
                          Nom    : in String;
                          Taille : in Integer) is
   begin
      Close_Font(F);
      F.Fon := C_Open_Font(To_C(Nom), Int(Taille));
      if F.Fon = Null_SDL_Font then
         Ada.Exceptions.Raise_Exception(Font_Error'Identity,"Impossible de charger la police " & Nom & " avec la taille" & Integer'Image(Taille) & " : " & Game.Error);
      end if;
   end Open_Font;

   ---------------------------
   -- INITIALIZE - FINALIZE --
   ---------------------------

   procedure Initialize (F : in out Font) is
   begin
      F.Fon := Null_SDL_Font;
   end Initialize;

   procedure Finalize (F : in out Font) is
   begin
      Close_Font(F);
   end Finalize;

   -----------------------------
   -- CREATE_SURF (C VERSION) --
   -----------------------------

   function C_Create_Surf_Pol(Text      : in Char_Array;
                              F         : in SDL_Font;
                              F_Type    : in Int;
                              F_Encode  : in Int;
                              Normal    : in Int;
                              Bold      : in Int;
                              Italic    : in Int;
                              Underline : in Int;
                              Coul      : in C_Color;
                              Fond      : in C_Color) return SDL_Surface;
   pragma Import(C,C_Create_Surf_Pol,"create_text_font");

   -------------------
   -- GET_TEXT_SURF --
   -------------------

   function Get_Text_Surf (Text      : in String;
                           Police    : in Font;
                           F_Type    : in Type_Rendu := BLENDED;
                           Bold      : in Boolean    := False;
                           Underline : in Boolean    := False;
                           Italic    : in Boolean    := False;
                           Encode    : in Encodage   := UTF8;
                           Coul      : in Color      := (0,0,0);
                           Fond      : in Color      := (255,255,255)) return Surface is
      No : Int := 1;
      Res : SDL_Surface := Null_SDL_Surface;

   begin
      if Bold or Underline or Italic then
         No := 0;
      end if;
      Res := C_Create_Surf_Pol(Text      => To_C(Text),
                               F         => Police.Fon,
                               F_Type    => Type_Rendu'Pos(F_Type),
                               F_Encode  => Encodage'Pos(Encode),
                               Normal    => No,
                               Bold      => Boolean'Pos(Bold),
                               Italic    => Boolean'Pos(Italic),
                               Underline => Boolean'Pos(Underline),
                               Coul      => To_C_Col(Coul),
                               Fond      => To_C_Col(Fond));

      if Res = Null_SDL_Surface then
         Ada.Exceptions.Raise_Exception(Font_Error'Identity,"Impossible de créer la surface avec le texte " & Text & " : " & Game.Error);
      end if;

      return (AF.Controlled with Res);
   end Get_Text_Surf;

   function Get_Text_Surf(Text      : in String;
                          Police    : in String;
                          Taille    : in Natural;
                          F_Type    : in Type_Rendu := BLENDED;
                          Bold      : in Boolean    := False;
                          Underline : in Boolean    := False;
                          Italic    : in Boolean    := False;
                          Encode    : in Encodage   := UTF8;
                          Coul      : in Color      := (0,0,0);
                          Fond      : in Color      := (255,255,255)) return Surface is
      F : Font;
   begin
      Open_Font(F,Police,Taille);
      return Get_Text_Surf (Text      => Text,
                            Police    => F,
                            F_Type    => F_Type,
                            Bold      => Bold,
                            Underline => Underline,
                            Italic    => Italic,
                            Encode    => Encode,
                            Coul      => Coul,
                            Fond      => Fond);
   end Get_Text_Surf;

   ----------------
   -- PRINT_TEXT --
   ----------------

   procedure Print_Text(Surf      : in out Surface;
                        Pos       : in out Rect;
                        Text      : in     String;
                        Police    : in     Font;
                        F_Type    : in     Type_Rendu := BLENDED;
                        Bold      : in     Boolean    := False;
                        Underline : in     Boolean    := False;
                        Italic    : in     Boolean    := False;
                        Encode    : in     Encodage   := UTF8;
                        Coul      : in     Color      := (0,0,0);
                        Fond      : in     Color      := (255,255,255)) is
      S : Surface := Null_Surface;
   begin
      S := Get_Text_Surf(Text,Police,F_Type,Bold,Underline,Italic,Encode,Coul,Fond);
      Blit(Surf,S,Pos);
      Flip;
      Pos.W := Get_Width(S);
      Pos.H := Get_Height(S);
   end Print_Text;

   procedure Print_Text(Surf      : in out Surface;
                        Pos       : in out Rect;
                        Text      : in     String;
                        Police    : in     String;
                        Taille    : in     Natural;
                        F_Type    : in     Type_Rendu := BLENDED;
                        Bold      : in     Boolean    := False;
                        Underline : in     Boolean    := False;
                        Italic    : in     Boolean    := False;
                        Encode    : in     Encodage   := UTF8;
                        Coul      : in     Color      := (0,0,0);
                        Fond      : in     Color      := (255,255,255))is
      F : Font;
   begin
      Open_Font(F,Police,Taille);
      Print_Text(Surf,Pos,Text,F,F_Type,Bold,Underline,Italic,Encode,Coul,Fond);
   end Print_Text;

end Game.Display.Font;
