--------------------------------------------
--                                        --
--       PACKAGE GAME - PARTIE ADA        --
--                                        --
--            GAME-GEVENT.ADB             --
--                                        --
--         Gestion des évènements         --
--                                        --
-- Créateur : CAPELLE Mikaël              --
-- Adresse  : capelle.mikael@gmail.com    --
--                                        --
-- Dernière modification : 14 / 06 / 2011 --
--                                        --
--------------------------------------------

with Interfaces.C;
use Interfaces.C;

package body Game.GEvent is

   ---------------
   -- NEW_EVENT --
   ---------------

   function New_Event return Event is
   begin
      return (NONE,(others => False),-1,-1,0,0,(others => False));
   end New_Event;

   ----------------------------
   -- TYPE EVENT (C VERSION) --
   ----------------------------

   type C_Event is
      record
         Etype        : Int;  -- Type de l'évènement sous forme d'int
         Ekey         : Int;  -- Touche du clavier sous forme d'entier
         X,Y          : Int;  -- Coordonnées du clic
         X_Rel, Y_Rel : Int;  -- Coordonnées relative du mouvement
         Embutton     : Int;  -- Bouton de la souris
      end record;
   pragma Convention(C,C_Event);

   function C_Wevent return C_Event;
   pragma Import(C,C_Wevent,"wait_event");

   procedure C_Pevent(CE : in out C_Event; End_Of_Queue : in out Int);
   pragma Import(C,C_Pevent,"poll_event");

   -- Transforme un event venant du C sous forme C_Event en Event Ada
   procedure Change_Event(E   : in out Event;
                          Tmp : in     C_Event) is
   begin
      -- Conversion du type de l'event
      E.Etype := Event_Type'Val(Tmp.Etype);
      case E.Etype is
         when KEYDOWN           =>
            E.Ekey(Event_Key'Val(Tmp.Ekey)) := True;
         when KEYUP             =>
            E.Ekey(Event_Key'Val(Tmp.Ekey)) := False;
         when MOUSE_BUTTON_DOWN =>
            E.Emousebutton(Event_Mouse_Key'Val(Tmp.Embutton)) := True;
            E.X := Integer(Tmp.X);
            E.Y := Integer(Tmp.Y);
         when MOUSE_BUTTON_UP   =>
            E.Emousebutton(Event_Mouse_Key'Val(Tmp.Embutton)) := False;
            E.X := Integer(Tmp.X);
            E.Y := Integer(Tmp.Y);
         when MOUSE_MOTION      =>
            E.X := Integer(Tmp.X);
            E.Y := Integer(Tmp.Y);
            E.X_Rel := Integer(Tmp.X_Rel);
            E.Y_Rel := Integer(Tmp.Y_Rel);
         when others            => null;
      end case;
   end Change_Event;

   ----------------
   -- WAIT_EVENT --
   ----------------

   procedure Wait_Event (E : in out Event) is
   begin
      -- Appel de la fonction C que l'on convertit pour la mettre dans E
      Change_Event(E,C_Wevent);
   end Wait_Event;

   ----------------
   -- POLL_EVENT --
   ----------------

   procedure Poll_Event(E : in out Event; End_Of_Queue : out Boolean) is
      Tmp : C_Event := (0,0,0,0,0,0,0);
      End_Queue : Int := 0;
   begin
      -- Appel de la fonction C
      C_Pevent(Tmp,End_Queue);
      if End_Queue = 1 then
         -- Si évènement trouvé, convertion
         Change_Event(E,Tmp);
         End_Of_Queue := False;
      else
         End_Of_Queue := True;
      end if;
   end Poll_Event;

   ----------------
   -- KEY_REPEAT --
   ----------------

   procedure C_Ekeyr(Wait, Interval : in Int);
   pragma Import(C,C_Ekeyr,"enable_key_repeat");

   procedure C_Dkeyr;
   pragma Import(C,C_Dkeyr,"disable_key_repeat");

   procedure Enable_Key_Repeat(Wait : in Positive := 500;
                               Interval : in Positive := 30) is
   begin
      C_Ekeyr(Int(Wait),Int(Interval));
   end Enable_Key_Repeat;

   procedure Disable_Key_Repeat is
   begin
      C_Dkeyr;
   end Disable_Key_Repeat;

end Game.GEvent;
