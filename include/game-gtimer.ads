--------------------------------------------
--                                        --
--       PACKAGE GAME - PARTIE ADA        --
--                                        --
--             GAME-GTIMER.ADS            --
--                                        --
--           Gestion des timers           --
--                                        --
-- Créateur : CAPELLE Mikaël              --
-- Adresse  : capelle.mikael@gmail.com    --
--                                        --
-- Dernière modification : 14 / 06 / 2011 --
--                                        --
--------------------------------------------

package Game.Gtimer is

   Max_Time : constant := 2**32 - 1 ;
   type Time is range 0 .. Max_Time ;

   -- Retourne le temps (en milliseconde) depuis l'initialisation
   function Get_Tick return Time;

   -- Mets le programme en pose pendant 'time' milliseconde
   procedure Wait (MS : in Time);

end Game.Gtimer;

