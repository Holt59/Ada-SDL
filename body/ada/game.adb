--------------------------------------------
--                                        --
--       PACKAGE GAME - PARTIE ADA        --
--                                        --
--               GAME.ADB                 --
--                                        --
--       Gestion des outils de base       --
--                                        --
-- Créateur : CAPELLE Mikaël              --
-- Adresse  : capelle.mikael@gmail.com    --
--                                        --
-- Dernière modification : 14 / 06 / 2011 --
--                                        --
--------------------------------------------

with Interfaces.C;      use Interfaces.C;
with Ada.Exceptions;    use Ada.Exceptions;
with Game.Gtype;        use Game.Gtype;
with Interfaces.C.Pointers;
with Ada.Strings.Unbounded;

package body Game is

   ------------
   -- ADJUST --
   ------------

   function C_Convert_Surf (S  : in SDL_Surface;
                            HW : in Int) return SDL_Surface;
   pragma Import(C,C_Convert_Surf,"convert_surface");

   procedure Adjust (S : in out Surface) is
   begin
      if S.Surf /= Null_SDL_Surface and S.Surf /= C_Screen then
         S.Surf := C_Convert_Surf (S.Surf, 1);
      end if;
   end Adjust;

   -----------------
   -- CHANGE_ICON --
   -----------------

   procedure C_SetIcon_Surf(S : in SDL_Surface);
   pragma Import(C,C_SetIcon_Surf,"change_icon_surface");
   procedure C_SetIcon_String(S : in Char_Array);
   pragma Import(C,C_SetIcon_String,"change_icon_string");

   procedure Change_Icon(Name : in String) is
   begin
      C_SetIcon_String(To_C(Name));
   end Change_Icon;

   procedure Change_Icon(Surf : in Surface) is
   begin
      C_SetIcon_Surf(Surf.Surf);
   end Change_Icon;

   --------------
   -- FINALIZE --
   --------------

   procedure Finalize (S : in out Surface) is
   begin
      if S.Surf /= Null_SDL_Surface and S.Surf /= C_Screen then
         Free_Surface(S);
      end if;
   end Finalize;

   ------------------
   -- FREE_SURFACE --
   ------------------

   procedure C_Free_Surface(S : in SDL_Surface);
   pragma Import (C,C_Free_Surface,"free_surface");

   procedure Free_Surface(S : in out Surface) is
   begin
      if S.Surf /= Null_SDL_Surface and S.Surf /= C_Screen then
         C_Free_Surface(S.Surf);
         S.Surf := null;
      end if;
   end Free_Surface;

   ----------
   -- INIT --
   ----------

   ----------------------
   -- T : Timer        --
   -- V : Video        --
   -- A : Audio        --
   -- C : Cdrom        --
   -- J : Joystick     --
   -- TTF : Font       --
   -- E : Everything   --
   -- F : ...          --
   ----------------------

   function C_Init(T,V,A,C,J,TTF,E,F : in Int) return Int;
   pragma Import(C,C_Init,"init");

   procedure Init(Timer : in Boolean := True;
                  Video : in Boolean := True;
                  Audio : in Boolean := True;
                  Font  : in Boolean := True;
                  Frequency : in Positive := 44100) is
      T : Int := 0;
      V : Int := 0;
      A : Int := 0;
      TTF : Int := 0;
      Result : Int := 0;
   begin
      if Timer then T   := 1; end if;
      if Video then V   := 1; end if;
      if Audio then A   := 1; end if;
      if Font  then TTF := 1; end if;

      Result := c_Init(T,V,A,0,0,TTF,0,Int(Frequency));

      if Result = -1 then
         Ada.Exceptions.Raise_Exception(Init_Error'Identity,"Erreur d'initialisation du module Game : " & Game.Error);
      elsif Result = -2 then
         Ada.Exceptions.Raise_Exception(Init_Error'Identity,"Erreur d'initialisation du module Font : " & Game.Error);
      elsif Result = -3 then
         Ada.Exceptions.Raise_Exception(Init_Error'Identity,"Erreur d'initialisation du module Audio : " & Game.Error);
      end if;
   end Init;

   ----------------
   -- INITIALIZE --
   ----------------

   procedure Initialize (S : in out Surface) is
   begin
      S.Surf := Null_SDL_Surface;
   end Initialize;

   ---------------
   -- SET_TITLE --
   ---------------

   procedure C_Set_Title(S : in Char_Array);
   pragma Import(C,C_Set_Title,"change_title");

   procedure Change_Title(Name : in String) is
   begin
      C_Set_Title(To_C(Name));
   end Change_Title;

   ----------
   -- QUIT --
   ----------

   procedure C_Quit;
   pragma Import(C,C_Quit,"quit");
   procedure Quit is
   begin
      C_Quit;
   end Quit;

   ------------
   -- ERRORS --
   ------------

   package Char_Ptrs is
      new Interfaces.C.Pointers (Index              => Interfaces.C.Size_T,
                                 Element            => Interfaces.C.Char,
                                 Element_Array      => Interfaces.C.Char_Array,
                                 Default_Terminator => Interfaces.C.Nul);

   use Ada.Strings.Unbounded;

   -- Types :
   --  1 - SDL
   --  2 - SDL_image
   --  3 - SDL_ttf
   --  4 - SDL_mixer

   procedure Set_Error(S : in Char_Array; Typ : in Int);
   pragma Import(C,Set_Error,"set_error");
   function Get_Error(Typ : in Int) return Char_Ptrs.Pointer;
   pragma Import(C,Get_Error,"get_error");

   function Error return String is
      Tmp : Unbounded_String;
   begin
      for T in 1..4 loop
         Tmp := To_Unbounded_String(To_Ada(Char_Ptrs.Value(Get_Error(Int(T)))));
         if Tmp /= "" then
            Set_Error(To_C(""),Int(T));
            exit;
         end if;
      end loop;
      return To_String(Tmp);
   end Error;

end Game;
