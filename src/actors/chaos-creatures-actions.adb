with Chaos.UI.Logging;

package body Chaos.Creatures.Actions is

   -------------------
   -- Healing_Surge --
   -------------------

   procedure Healing_Surge
     (Creature : Chaos.Actors.Chaos_Actor)
   is
      Pts : Natural := Maximum_Hit_Points (Creature) / 4;
   begin
      Heal_Damage (Creature, Pts);
      Chaos.UI.Logging.Current_Logger.Log_Healing (Creature, Pts);
   end Healing_Surge;

end Chaos.Creatures.Actions;
