--------------------------------------------
--                                        --
--       PACKAGE GAME - PARTIE ADA        --
--                                        --
--            GAME-DISPLAY.ADB            --
--                                        --
--     Gestion de l'affichage principal   --
--                                        --
-- Créateur : CAPELLE Mikaël              --
-- Adresse  : capelle.mikael@gmail.com    --
--                                        --
-- Dernière modification : 14 / 06 / 2011 --
--                                        --
--------------------------------------------

with Ada.Exceptions;
with Interfaces.C;             use Interfaces.C;
with Interfaces.C.Extensions;  use Interfaces.C.Extensions;
with Ada.Finalization;         use Ada.Finalization;

package body Game.Display is

   ----------------
   -- GET_SCREEN --
   ----------------

   -- Actuellement cette fonction n'est plus utilisée

   function C_Get_Screen return SDL_Surface;
   pragma Import(C,C_Get_Screen,"get_screen");

   function Get_Screen return Surface is
   begin
      -- Si le mode video n'est pas chargé, C_Screen est nul => Erreur !
      if C_Screen = Null_SDL_Surface then
         Ada.Exceptions.Raise_Exception(Surface_Error'Identity,"Tentative d'accès à la surface écran (Get_Screen) sans mode vidéo.");
      end if;
      return (AF.Controlled with C_Screen);
   end Get_Screen;

   --------------------
   -- CREATE_SURFACE --
   --------------------

   function C_Create_Rect(W,H,P,HW : in Int) return SDL_Surface;
   pragma Import(C,C_Create_Rect,"create_rectangle");

   function Create_Rectangle (Width     : in Size;
                              Height    : in Size;
                              Depth     : in Profondeur := 32;
                              VRAM      : in Boolean := True) return Surface is
      Int_Vram : Int := 0;
      Res : SDL_Surface := Null_SDL_Surface;
   begin
      if VRAM then
         Int_Vram := 1;
      end if;
      Res := C_Create_Rect(Int(Width),Int(Height),Int(Depth),Int_Vram);
      if Res = Null_SDL_Surface then
         Ada.Exceptions.Raise_Exception(Surface_Error'Identity,"Impossible de créer la surface de" & Size'Image(Width) & " x" & Size'Image(Height) & " : " & Game.Error);
      end if;
      return (AF.Controlled with Res);
   end Create_Rectangle;

   ----------------
   -- LOAD_IMAGE --
   ----------------

   function C_Load_Img(S : in Char_Array) return SDL_Surface;
   pragma Import(C,C_Load_Img,"load_image");

   function Load_Image(Name : in String) return Surface is
      Res : SDL_Surface := Null_SDL_Surface;
   begin
      Res := C_Load_Img(To_C(Name));
      if Res = Null_SDL_Surface then
         Ada.Exceptions.Raise_Exception(Surface_Error'Identity,"Impossible de charger l'image " & '"' & Name & '"' & " : " & Game.Error);
      end if;

      return (AF.Controlled with Res);
   end Load_Image;

   ----------------
   -- SAVE_IMAGE --
   ----------------

   procedure C_Save (S : in SDL_Surface; Name : in Char_Array);
   pragma Import(C,C_Save,"save_image");

   procedure Save_Image(Surf : in Surface;
                        Name : in String) is
   begin
      C_Save(Surf.Surf,To_C(Name));
   end Save_Image;

   ----------
   -- BLIT --
   ----------

   function C_Blit(Dst,Src : in SDL_Surface; Pos,Part : in C_Rect; No_Pos, Full_Img : in Int) return Int;
   pragma Import (C,C_Blit,"blit_image");

   procedure Blit(Destination  : in out Surface;
                  Source       : in     Surface) is
      Success : Int := 0;
   begin
      Success := C_Blit(Dst => Destination.Surf,
                        Src => Source.Surf,
                        Pos => (0,0,0,0),
                        Part => (0,0,0,0),
                        No_Pos => 1,
                        Full_Img => 1);
      if Success = 0 then
         Ada.Exceptions.Raise_Exception(Surface_Error'Identity,"Blit impossible : " & Game.Error);
      end if;
   end Blit;

   procedure Blit(Destination  : in out Surface;
                  Source       : in     Surface;
                  Position     : in     Rect) is
      Success : Int := 0;
   begin
      Success := C_Blit(Dst => Destination.Surf,
                        Src => Source.Surf,
                        Pos => To_C_Rect(Position),
                        Part => (0,0,0,0),
                        No_Pos => 0,
                        Full_Img => 1);
      if Success = 0 then
         Ada.Exceptions.Raise_Exception(Surface_Error'Identity,"Blit impossible : " & Game.Error);
      end if;
   end Blit;

   procedure Blit(Destination  : in out Surface;
                  Source       : in     Surface;
                  Position     : in     Rect;
                  Partie       : in     Rect) is
      Success : Int := 0;
   begin
      Success := C_Blit(Dst => Destination.Surf,
                        Src => Source.Surf,
                        Pos => To_C_Rect(Position),
                        Part => To_C_Rect(Partie),
                        No_Pos => 0,
                        Full_Img => 0);
      if Success = 0 then
         Ada.Exceptions.Raise_Exception(Surface_Error'Identity,"Blit impossible : " & Game.Error);
      end if;
   end Blit;

   ----------
   -- FILL --
   ----------

   function C_Fill(S : in SDL_Surface; Re : in C_Rect; R,G,B : in Int; Full : in Int) return Int;
   pragma Import(C,C_Fill,"fill");

   procedure Fill(Surf : in Surface;
                  Col  : in Color) is
   begin
      if C_Fill(Surf.Surf,(0,0,0,0),Int(Col.R),Int(Col.G),Int(Col.B),1) = -1 then
         Ada.Exceptions.Raise_Exception(Surface_Error'Identity,"Remplissage impossible : " & Game.Error);
      end if;
   end Fill;

   procedure Fill(Surf   : in Surface;
                  Pos    : in Rect;
                  Col    : in Color) is
   begin
      if C_Fill(Surf.Surf,To_C_Rect(Pos),Int(Col.R),Int(Col.G),Int(Col.B),0) = -1 then
         Ada.Exceptions.Raise_Exception(Surface_Error'Identity,"Remplissage impossible : " & Game.Error);
      end if;
   end Fill;

   ----------
   -- FLIP --
   ----------

   procedure C_Flip;
   pragma Import(C,C_Flip,"flip");

   procedure Flip is
   begin
      C_Flip;
   end Flip;

   --------------------
   -- DISPLAY_FORMAT --
   --------------------

   function C_Display_Format(S : in SDL_Surface; Alpha : in Int) return SDL_Surface;
   pragma Import(C,C_Display_Format,"display_format");

   function Display_Format(Surf  : in Surface;
                           Alpha : in Boolean := False) return Surface is
      A : Int := 0;
   begin
      if Alpha then
         A := 1;
      end if;
      return (Controlled with C_Display_Format(Surf.Surf,A));
   end Display_Format;

   ---------------
   -- SET_VIDEO --
   ---------------
   function C_Set_Video(L,H : in Int; Prof : in Int; Hw,Dbl_Buff,Full_Screen,Resizable,No_Frame : in Int) return SDL_Surface;
   pragma Import(C,C_Set_Video,"set_video");

   procedure Set_Video(Width            : in Size;
                       Height           : in Size;
                       Depth            : in Profondeur := 32;
                       VRAM             : in Boolean := True;
                       Full_Screen      : in Boolean := False;
                       Resizable        : in Boolean := False;
                       No_Frame         : in Boolean := False;
                       Double_Buffering : in Boolean := True) is
   begin
      C_Screen := c_Set_Video(L => Int(Width),
                              H => Int(Height),
                              Prof => Int(Depth),
                              Hw => Boolean'Pos(VRAM),
                              Dbl_Buff => Boolean'Pos(Double_Buffering),
                              Full_Screen => Boolean'Pos(Full_Screen),
                              Resizable => Boolean'Pos(Resizable),
                              No_Frame => Boolean'Pos(No_Frame));
      if C_Screen = Null_SDL_Surface then
         Ada.Exceptions.Raise_Exception(Video_Error'Identity,"Impossible de lancer le mode video : " & Game.Error);
      end if;
   end Set_Video;

   -------------------
   -- SET_COLOR_KEY --
   -------------------

   procedure C_Set_Color_Key(S : in SDL_Surface; Flag : in Int; R,G,B : in Int);
   pragma Import(C,C_Set_Color_Key,"set_color_key");

   procedure Set_Color_Key(Surf : in Surface;
                           Col  : in Color;
                           Flag : in Active := ENABLE) is
      Tmp : Int := 0;
   begin
      if Flag = ENABLE then
         Tmp := 1;
      else
         Tmp := 0;
      end if;
      C_Set_Color_Key(Surf.Surf,Tmp,Int(Col.R),Int(Col.G),Int(Col.B));
   end Set_Color_Key;

   -----------------
   -- SHOW_CURSOR --
   -----------------

   procedure C_Show_Cursor(X : in Int);
   pragma Import(C,C_Show_Cursor,"show_cursor");

   procedure Show_Cursor(Flag : in Active := ENABLE) is
   begin
      if Flag = ENABLE then
         C_Show_Cursor(1);
      else
         C_Show_Cursor(0);
      end if;
   end Show_Cursor;

end Game.Display;
