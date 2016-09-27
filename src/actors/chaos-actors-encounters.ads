package Chaos.Actors.Encounters is

   procedure Start_Encounter
     (Creature : Chaos.Actors.Chaos_Actor);

   procedure End_Encounter
     (Creature : Chaos.Actors.Chaos_Actor);

   procedure Start_Round
     (Creature : Chaos.Actors.Chaos_Actor);

   procedure End_Round
     (Creature : Chaos.Actors.Chaos_Actor);

   procedure Start_Turn
     (Creature : Chaos.Actors.Chaos_Actor);

   procedure End_Turn
     (Creature : Chaos.Actors.Chaos_Actor);

end Chaos.Actors.Encounters;
