--------------------------------------------
--                                        --
--       PACKAGE GAME - PARTIE ADA        --
--                                        --
--             GAME-GTIMER.ADB            --
--                                        --
--           Gestion des timers           --
--                                        --
-- Créateur : CAPELLE Mikaël              --
-- Adresse  : capelle.mikael@gmail.com    --
--                                        --
-- Dernière modification : 14 / 06 / 2011 --
--                                        --
--------------------------------------------

with Interfaces.C.Extensions;
use Interfaces.C.Extensions;

package body Game.Gtimer is

   --------------
   -- GET_TICK --
   --------------

   function C_Get_Tick return Unsigned_32;
   pragma Import(C,C_Get_Tick,"get_tick");
   function Get_Tick return Time is
   begin
      return Time(C_Get_Tick);
   end Get_Tick;

   ----------
   -- WAIT --
   ----------

   procedure C_Wait(Ms : in Unsigned_32);
   pragma Import(C,C_Wait,"wait");
   procedure Wait(MS : in Time) is
   begin
     C_Wait(Unsigned_32(MS));
   end Wait;

end Game.Gtimer;
