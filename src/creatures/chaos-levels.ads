package Chaos.Levels is

   type Chaos_Level is range 0 .. 30;

   type Chaos_Level_Interface is limited interface;

   function Level
     (Leveler : Chaos_Level_Interface)
      return Chaos_Level
      is abstract;

   function Level_Bonus
     (Leveler : Chaos_Level_Interface'Class)
      return Natural
   is (Natural (Leveler.Level) / 2);

end Chaos.Levels;
