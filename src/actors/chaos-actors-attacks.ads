with Chaos.Items.Weapons;
with Chaos.Powers;

package Chaos.Actors.Attacks is

   procedure Weapon_Attack
     (Attacker : Chaos.Actors.Chaos_Actor;
      Defender : Chaos.Actors.Chaos_Actor;
      Weapon   : Chaos.Items.Weapons.Chaos_Weapon)
     with Pre => Attacker.Has_Standard_Action,
     Post => not Attacker.Has_Standard_Action;

   procedure Power_Attack
     (Attacker      : Chaos.Actors.Chaos_Actor;
      Defender      : Chaos.Actors.Chaos_Actor;
      Power         : Chaos.Powers.Chaos_Power)
     with Pre => Attacker.Has_Standard_Action,
     Post => not Attacker.Has_Standard_Action;

   procedure Power_Attack
     (Attacker      : Chaos.Actors.Chaos_Actor;
      Target        : Chaos.Locations.Square_Location;
      Power         : Chaos.Powers.Chaos_Power)
     with Pre => Attacker.Has_Standard_Action,
     Post => not Attacker.Has_Standard_Action;

end Chaos.Actors.Attacks;
