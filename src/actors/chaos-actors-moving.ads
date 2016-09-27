package Chaos.Actors.Moving is

   function Can_Move_To
     (Creature : Chaos.Actors.Chaos_Actor;
      Location : Chaos.Locations.Square_Location)
      return Boolean;

end Chaos.Actors.Moving;
