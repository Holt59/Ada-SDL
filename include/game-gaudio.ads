--------------------------------------------
--                                        --
--       PACKAGE GAME - PARTIE ADA        --
--                                        --
--           GAME-GAUDIO.ADS              --
--                                        --
--   Gestion du son (musique et "chunk")  --
--                                        --
-- Créateur : CAPELLE Mikaël              --
-- Adresse  : capelle.mikael@gmail.com    --
--                                        --
-- Dernière modification : 14 / 06 / 2011 --
--                                        --
--------------------------------------------

with Interfaces.C;
with Ada.Finalization;

package Game.gAudio is

   -- Types servant à stocker des Musique ou des sons court
   type Music is limited private;
   type Chunk is limited private;

   Max_Volume : constant := 128;
   type Volume is range 0..Max_Volume;


   ----------------------------------------
   -- FONCTIONS DE GESTION DE LA MUSIQUE --
   ----------------------------------------

   -- Format supporté (en théorie, si erreur merci de me contacter) :
   --     .wav
   --     .aiff
   --     .voc
   --     .mod .xm .s3m .669 .it .med
   --     .mid
   --     .ogg
   --     .mp3

   -- Charge une musique
   procedure Load_Music (Mus  : out Music;
                         Name : in String);

   -- Joue la musique
   -- Une seule musique peut être joué à la fois
   procedure Play_Music(Mus   : in Music;               -- Musique à jouer
                        Loops : in Boolean  := True;    -- En boucle ?
                        Nb    : in Positive := 1);      -- Nombre de fois (si pas de boucle)

   -- Arrête la musique
   procedure Stop_Music;

   -- Met la musique en Pause si P vaut True, sinon enlève la pause
   procedure Pause_Music(P : in Boolean := True);

   -- Vérifie si la musique est en pause ou en train de joué
   function Music_Paused  return Boolean;
   function Music_Playing return Boolean;

   -- Recommence la musique depuis le début
   procedure Restart_Music;

   -- Change le volume de la musique
   procedure Set_Music_Volume (V : in Volume);


   -------------------------------------------------
   -- FONCTION DE GESTION DES SONS COURTS (CHUNK) --
   -------------------------------------------------

   -- Chaque son court est joué sur un canal (channel) différent
   -- Pour affecter un son court, vous devez affecter le canal sur
   -- lequel il est joué

   -- Format supporté (en théorie, si erreur merci de me contacter) :
   --     .wav
   --     .aiff
   --     .voc
   --     .riff
   --     .ogg

   -- Type Canal, vous pouvez avoir maximum 100 canaux (<=> sons) ouvert en même temps
   Last_Channel : constant := 100 ;
   type Channel is range 0 .. Last_Channel ;

   -- Charge un son court
   procedure Load_Chunk (Ch    : out Chunk;
                         Name  : in String);

   -- Alloue Nb canaux
   procedure Allocate_Channel (Nb : in Positive);

   -- Joue le son Ch
   -- Premier canal libre utilisé dans le premier cas
   procedure Play_Channel(Ch   : in Chunk;          -- Chunk à joeur
                          Nb   : in Positive := 1;  -- Nombre de fois que le chunk est joué
                          Time : in Natural  := 0); -- Temps du chunk (si 0, le chunk est joué en entier)

   procedure Play_Channel(Ch   : in Chunk;          -- Chunk à jouer
                          Chan : in Channel;        -- Canal sur lequel jouer le chunk
                          Nb   : in Positive := 1;  -- Nombre de fois que le chunk est joué
                          Time : in Natural  := 0); -- Temps du chunk (si 0, le chunk est joué en entier)

   -- Stop le canal Ch, pareil qu'au dessus si aucun canal n'est spécifié
   procedure Stop_Channel;
   procedure Stop_Channel(Ch : in Channel);

   -- Met en pause le canal Ch, s'il n'est pas spécifié, met en pause tous les canaux
   procedure Pause_Channel(P : in Boolean := True);
   procedure Pause_Channel(Chan : in Channel;
                           P    : in Boolean := True);

   -- Test si le canal est en pause / en train de joué
   function Channel_Paused (Ch : in Channel) return Boolean;
   function Channel_Playing(Ch : in Channel) return Boolean;

   -- Retourne le nombre de canaux en pause / en train de joué
   function Nb_Chan_Paused  return Natural;
   function Nb_Chan_Playing return Natural;

   -- Met le volume du canal C à V
   procedure Set_Channel_Volume(V : in Volume);
   procedure Set_Channel_Volume(C : in Channel; V : in Volume);

private

   package AF renames Ada.Finalization;

   -----------
   -- MUSIC --
   -----------

   type SDL_Music is access all Interfaces.C.Int;

   Null_SDL_Music : constant SDL_Music := null;

   procedure Close_Music (M : in out Music);

   type Music is new AF.Controlled with
      record
         Mus : SDL_Music := Null_SDL_Music;
      end record;

   procedure Initialize (M : in out Music);
   procedure Finalize   (M : in out Music);

   -----------
   -- CHUNK --
   -----------

   type SDL_Chunk is access all Interfaces.C.Int;

   Null_SDL_Chunk : constant SDL_Chunk := null;

   procedure Close_Chunk (C : in out Chunk);

   type Chunk is new AF.Controlled with
      record
         Chu : SDL_Chunk := Null_SDL_Chunk;
      end record;

   procedure Initialize (C : in out Chunk);
   procedure Finalize   (C : in out Chunk);

end Game.gAudio;
