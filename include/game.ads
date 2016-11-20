--------------------------------------------
--                                        --
--       PACKAGE GAME - PARTIE ADA        --
--                                        --
--               GAME.ADS                 --
--                                        --
--       Gestion des outils de base       --
--                                        --
-- Créateur : CAPELLE Mikaël              --
-- Adresse  : capelle.mikael@gmail.com    --
--                                        --
-- Dernière modification : 14 / 06 / 2011 --
--                                        --
--------------------------------------------

with Interfaces.C;        use Interfaces.C;
with Ada.Finalization;

package Game is

   -- Les différentes erreurs pouvant être levé par la librairie
   -- Elles sont en générales accompagnés d'une instruction détaillé (pour plus d'info, voir
   -- le package Ada.Exceptions)
   -- La fonction error est (en général) appelé directement lorsque ces exceptions sont levées,
   -- mais vous pouvez l'utilisez vous même si vous en avez besoin
   Init_Error     : exception;
   Video_Error    : exception;
   Surface_Error  : exception;
   Audio_Error    : exception;
   Font_Error     : exception;
   Draw_Error     : exception;

   -- Type servant à stocker des surfaces graphique
   -- ne peut être utilisé directement, vous devez passer par les fonctions
   type Surface is private;
   Null_Surface   : constant Surface;

   -- Initialise la librairie
   -- Sans argument la fonction initialise tout
   -- Lève l'exception Init_Error si une erreur survient
   procedure Init(Timer     : in Boolean := True;
                  Video     : in Boolean := True;
                  Audio     : in Boolean := True;
                  Font      : in Boolean := True;
                  Frequency : in Positive := 44100);

   -- Ferme la librairie (à utiliser à la fin du programme)
   procedure Quit;

   -- Change le titre ou l'icone de la fenêtre
   procedure Change_Title (Name : in String);
   procedure Change_Icon  (Name : in String);
   procedure Change_Icon  (Surf : in Surface);

   -- Retourne une indication sur la dernière erreur survenue
   function Error return String;

private

   package AF renames Ada.Finalization;

   type SDL_Surface is access all Int;

   Null_SDL_Surface : constant SDL_Surface := null;
   C_Screen         :          SDL_Surface := Null_SDL_Surface;  -- Ecran principale (pour faciliter Get_Screen)


   -- Libère la mémoire alloué par une surface
   procedure Free_Surface (S : in out Surface);

   type Surface is new AF.Controlled with
      record
         Surf : SDL_Surface;
      end record;

   procedure Initialize (S : in out Surface);
   procedure Adjust (S : in out Surface);
   procedure Finalize (S : in out Surface);

   Null_Surface   : constant Surface := (AF.Controlled with Null_SDL_Surface);

end Game;
