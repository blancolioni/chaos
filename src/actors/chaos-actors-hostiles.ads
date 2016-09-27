package Chaos.Actors.Hostiles is

   function Adjacent_Hostile
     (Actor    : Chaos.Actors.Chaos_Actor;
      Location : Chaos.Locations.Square_Location)
      return Boolean;
   --  is there a location adjacent to Location which contains a
   --  creature hostile to Creature?

   function Adjacent_Hostile
     (Actor : Chaos.Actors.Chaos_Actor)
      return Boolean
   is (Adjacent_Hostile (Actor, Actor.Location));
   --  is there a hostile creature adjacent to Creature's current location?

   function Visible_Hostile
     (Actor : Chaos.Actors.Chaos_Actor)
      return Boolean;
   --  can the creature see a hostile creature?

   function Visible_Hostile_Player
     (Actor : Chaos.Actors.Chaos_Actor)
      return Boolean;
   --  can the creature see a hostile player-controlled creature?

end Chaos.Actors.Hostiles;
