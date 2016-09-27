package Chaos.Creatures.Configure is

   procedure Set_Alignment
     (Creature  : in out Chaos_Creature_Record'Class;
      Alignment : Chaos.Alignment.Chaos_Alignment);

   procedure Set_Race
     (Creature : in out Chaos_Creature_Record'Class;
      Race     : Chaos.Races.Chaos_Race);

   procedure Set_Class
     (Creature : in out Chaos_Creature_Record'Class;
      Class    : Chaos.Classes.Chaos_Class);

   procedure Set_Abilities
     (Creature  : in out Chaos_Creature_Record'Class;
      Abiliites : Chaos.Abilities.Ability_Scores);

end Chaos.Creatures.Configure;
