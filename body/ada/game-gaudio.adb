--------------------------------------------
--                                        --
--       PACKAGE GAME - PARTIE ADA        --
--                                        --
--           GAME-GAUDIO.ADB              --
--                                        --
--   Gestion du son (musique et "chunk")  --
--                                        --
-- Créateur : CAPELLE Mikaël              --
-- Adresse  : capelle.mikael@gmail.com    --
--                                        --
-- Dernière modification : 14 / 06 / 2011 --
--                                        --
--------------------------------------------

with Interfaces.C;    use Interfaces.C;
with Ada.Exceptions;

package body Game.gAudio is

   -----------
   -- MUSIC --
   -----------

   ------------------
   -- FINALIZATION --
   ------------------

   procedure C_Close_Music(M : in SDL_Music);
   pragma Import(C,C_Close_Music,"close_music");

   procedure Close_Music (M : in out Music) is
   begin
      if M.Mus /= Null_SDL_Music then
         C_Close_Music(M.Mus);
         M.Mus := Null_SDL_Music;
      end if;
   end Close_Music;

   procedure Initialize (M : in out Music) is
   begin
      M.Mus := Null_SDL_Music;
   end Initialize;

   procedure Finalize   (M : in out Music) is
   begin
      Close_Music(M);
   end Finalize;

   ----------------
   -- LOAD_MUSIC --
   ----------------

   function C_Load_Music(S : in Char_Array) return SDL_Music;
   pragma Import(C,C_Load_Music,"load_music");


   procedure Load_Music (Mus  : out Music;
                         Name : in String) is
   begin
      Close_Music (Mus);
      Mus.Mus := C_Load_Music(To_C(Name));
      if Mus.Mus = Null_SDL_Music then
         Ada.Exceptions.Raise_Exception(Audio_Error'Identity,"Impossible de charger la musique : " & Name & " : " & Game.Error);
      end if;
   end Load_Music;

   ----------------
   -- PLAY_MUSIC --
   ----------------

   function C_Play_Music(M     : in SDL_Music;
                         Loops : in Int) return Int;
   pragma Import(C,C_Play_Music,"play_music");

   procedure Play_Music(Mus   : in Music;
                        Loops : in Boolean  := True;
                        Nb    : in Positive := 1) is
      Result : Int := 0;
   begin
      if Loops then
         Result := C_Play_Music(Mus.Mus,-1);
      else
         Result := C_Play_Music(Mus.Mus,Int(Nb));
      end if;
      if Result = -1 then
         Ada.Exceptions.Raise_Exception(Audio_Error'Identity,"Impossible de jouer la musique : " & Game.Error);
      end if;
   end Play_Music;

   ----------------------
   -- SET_MUSIC_VOLUME --
   ----------------------

   function C_Set_Music_Volume(V : in Int) return Int;
   pragma Import(C,C_Set_Music_Volume,"set_music_volume");

   procedure Set_Music_Volume(V : in Volume) is
      Tmp : Int := 0;
   begin
      Tmp := C_Set_Music_Volume(Int(V));
   end Set_Music_Volume;

   -----------------
   -- PAUSE_MUSIC --
   -----------------

   procedure C_Pause_Music;
   pragma Import(C,C_Pause_Music,"pause_music");
   procedure C_Resume_Music;
   pragma Import(C,C_Resume_Music,"resume_music");

   procedure Pause_Music(P : in Boolean := True) is
   begin
      if P then
         C_Pause_Music;
      else
         C_Resume_Music;
      end if;
   end Pause_Music;

   -------------------
   -- RESTART_MUSIC --
   -------------------

   procedure C_Restart_Music;
   pragma Import(C,C_Restart_Music,"restart_music");

   procedure Restart_Music is
   begin
      C_Restart_Music;
   end Restart_Music;

   ----------------
   -- STOP_MUSIC --
   ----------------

   procedure C_Stop_Music;
   pragma Import(C,C_Stop_Music,"stop_music");

   procedure Stop_Music is
   begin
      C_Stop_Music;
   end Stop_Music;

   ----------------------------------
   -- MUSIC_PAUSED - MUSIC_PLAYING --
   ----------------------------------

   function C_Music_Paused return Int;
   pragma Import(C,C_Music_Paused,"music_paused");
   function C_Music_Playing return Int;
   pragma Import(C,C_Music_Playing, "music_playing");

   function Music_Paused return Boolean is
      Tmp : Int := 0;
   begin
      Tmp := C_Music_Paused;
      if Tmp = 0 then
         return False;
      else
         return True;
      end if;
   end Music_Paused;

   function Music_Playing return Boolean is
      Tmp : Int := 0;
   begin
      Tmp := C_Music_Playing;
      if Tmp = 0 then
         return False;
      else
         return True;
      end if;
   end Music_Playing;

   -----------
   -- CHUNK --
   -----------


   ------------------
   -- FINALIZATION --
   ------------------

   procedure C_Close_Chunk(S : in SDL_Chunk);
   pragma Import(C,C_Close_Chunk,"close_chunk");


   procedure Close_Chunk (C : in out Chunk) is
   begin
      if C.Chu /= Null_SDL_Chunk then
         C_Close_Chunk(C.Chu);
         C.Chu := Null_SDL_Chunk;
      end if;
   end Close_Chunk;

   procedure Initialize (C : in out Chunk) is
   begin
      C.Chu := Null_SDL_Chunk;
   end Initialize;

   procedure Finalize   (C : in out Chunk) is
   begin
      Close_Chunk(C);
   end Finalize;

   ----------------
   -- LOAD_CHUNK --
   ----------------

   function C_Load_Chunk(S : in Char_Array) return SDL_Chunk;
   pragma Import(C,C_Load_Chunk,"load_chunk");

   procedure Load_Chunk (Ch    : out Chunk;
                         Name  : in String) is
   begin
      Close_Chunk(Ch);
      Ch.Chu := C_Load_Chunk(To_C(Name));
      if Ch.Chu = Null_SDL_Chunk then
         Ada.Exceptions.Raise_Exception(Audio_Error'Identity,"Impossible de charger le son " & Name & " : " & Game.Error);
      end if;
   end Load_Chunk;

   ----------------------
   -- ALLOCATE_CHANNEL --
   ----------------------

   function C_Alloc_Chan(Nb : in Int) return Int;
   pragma Import(C,C_Alloc_Chan,"alloc_chan");

   procedure Allocate_Channel(Nb : in Positive) is
      Tmp : Int := 0;
   begin
      Tmp := C_Alloc_Chan(Int(Nb));
      if Tmp /= Int(Nb) then
         Ada.Exceptions.Raise_Exception(Audio_Error'Identity,"Impossible d'alloué les" & Integer'Image(Nb) & " canaux." & Positive'Image(Nb-Positive(Tmp)) & " non alloué(s) : " & Game.Error);
      end if;
   end Allocate_Channel;

   ------------------
   -- PLAY_CHANNEL --
   ------------------

   function C_Play_Channel(Chan : in Int; Ch : in SDL_Chunk; Nb : in Int; Time : in Int) return Int;
   pragma Import(C,C_Play_Channel,"play_chan");

   procedure Play_Channel(Ch   : in Chunk;
                          Nb   : in Positive := 1;
                          Time : in Natural  := 0) is
      Tmp : Int := 0;
   begin
      if Time = 0 then
         Tmp := C_Play_Channel(-1,Ch.Chu,Int(Nb),-1);
      else
         Tmp := C_Play_Channel(-1,Ch.Chu,Int(Nb),Int(Time));
      end if;
      if Tmp = -1 then
         Ada.Exceptions.Raise_Exception(Audio_Error'Identity,"Impossible de jouer le son sur le canal : " & Game.Error);
      end if;
   end Play_Channel;

   procedure Play_Channel(Ch   : in Chunk;
                          Chan : in Channel;
                          Nb   : in Positive := 1;
                          Time : in Natural  := 0) is
      Tmp : Int := 0;
   begin
      if Time = 0 then
         Tmp := C_Play_Channel(Int(Chan),Ch.Chu,Int(Nb),-1);
      else
         Tmp := C_Play_Channel(Int(Chan),Ch.Chu,Int(Nb),Int(Time));
      end if;
      if Tmp = -1 then
         Ada.Exceptions.Raise_Exception(Audio_Error'Identity,"Impossible de jouer le son sur le canal" & Channel'Image(Chan) & " : " & Game.Error);
      end if;
   end Play_Channel;

   -------------------
   -- PAUSE_CHANNEL --
   -------------------

   procedure C_Pause_Channel(C : in Int);
   pragma Import(C,C_Pause_Channel, "pause_chan");
   procedure C_Resume_Channel(C : in Int);
   pragma Import(C,C_Resume_Channel,"resume_chan");

   procedure Pause_Channel(P : in Boolean := True) is
   begin
      if P then
         C_Pause_Channel(-1);
      else
         C_Resume_Channel(-1);
      end if;
   end Pause_Channel;

   procedure Pause_Channel(Chan : in Channel;
                           P    : in Boolean := True) is
   begin
      if P then
         C_Pause_Channel(Int(Chan));
      else
         C_Resume_Channel(Int(Chan));
      end if;
   end Pause_Channel;

   ------------------
   -- STOP_CHANNEL --
   ------------------

   procedure C_Stop_Chan(Ch : in Int);
   pragma Import(C,C_Stop_Chan,"stop_chan");

   procedure Stop_Channel is
   begin
      C_Stop_Chan(-1);
   end Stop_Channel;

   procedure Stop_Channel(Ch : in Channel) is
   begin
      C_Stop_Chan(Int(Ch));
   end Stop_Channel;

   --------------------------------------
   -- CHANNEL_PAUSED - CHANNEL_PLAYING --
   --------------------------------------

   function C_Chan_Paused(Ch : in Int) return Int;
   pragma Import(C,C_Chan_Paused,"chan_paused");
   function C_Chan_Playing(Ch : in Int) return Int;
   pragma Import(C,C_Chan_Playing,"chan_playing");

   function Channel_Paused(Ch : in Channel) return Boolean is
      Tmp : Int := 0;
   begin
      Tmp := C_Chan_Paused(Int(Ch));
      if Tmp = 0 then
         return False;
      else
         return True;
      end if;
   end Channel_Paused;

   function Channel_Playing(Ch : in Channel) return Boolean is
      Tmp : Int := 0;
   begin
      Tmp := C_Chan_Playing(Int(Ch));
      if Tmp = 0 then
         return False;
      else
         return True;
      end if;
   end Channel_Playing;

   --------------------------------------
   -- NB_CHAN_PAUSED - NB_CHAN_PLAYING --
   --------------------------------------

   function Nb_Chan_Paused return Natural is
      Res : Int := 0;
   begin
      Res := C_Chan_Paused(-1);
      return Natural(Res);
   end Nb_Chan_Paused;

   function Nb_Chan_Playing return Natural is
      Res : Int := 0;
   begin
      Res := C_Chan_Playing(-1);
      return Natural(Res);
   end Nb_Chan_Playing;

   ------------------------
   -- SET_CHANNEL_VOLUME --
   ------------------------

   procedure C_Set_Chan_Volume(C : in Int; V : in Int);
   pragma Import(C,C_Set_Chan_Volume,"set_chan_volume");

   procedure Set_Channel_Volume(V : in Volume) is
   begin
      C_Set_Chan_Volume(-1,Int(V));
   end Set_Channel_Volume;

   procedure Set_Channel_Volume(C : in Channel; V : in Volume) is
   begin
      C_Set_Chan_Volume(Int(C),Int(V));
   end Set_Channel_Volume;

end Game.gAudio;
