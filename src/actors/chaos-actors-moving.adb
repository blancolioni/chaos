with Chaos.Features;

with Chaos.World.Map;

package body Chaos.Actors.Moving is

   -----------------
   -- Can_Move_To --
   -----------------

   function Can_Move_To
     (Creature : Chaos.Actors.Chaos_Actor;
      Location : Chaos.Locations.Square_Location)
      return Boolean
   is
      pragma Unreferenced (Creature);
      use type Chaos.Actors.Chaos_Actor;
      Occupier : constant Chaos.Actors.Chaos_Actor :=
                   At_Location (Location);
      Feature  : constant Chaos.Db.Feature_Reference :=
                   Chaos.World.Map.Feature (Location);
   begin
      if Occupier /= Chaos.Db.Null_Creature_Reference then
         return False;
      end if;

      if Chaos.Features.Blocking (Feature) then
         return False;
      end if;

      return True;

   end Can_Move_To;

end Chaos.Actors.Moving;
