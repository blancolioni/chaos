package body Chaos.Classes.Create is

   -----------------------
   -- Default_Abilities --
   -----------------------

   function Default_Abilities
     (Class : Chaos_Class)
      return Chaos.Abilities.Ability_Scores
   is
      use Chaos.Abilities;
      Scores : constant array (Class.Key_Abilities'Range)
        of Ability_Score_Range :=
          (16, 14, 13, 12, 11, 10);
   begin
      return Result : Ability_Scores do
         for I in Class.Key_Abilities'Range loop
            Result (Class.Key_Abilities (I)) := Scores (I);
         end loop;
      end return;
   end Default_Abilities;

end Chaos.Classes.Create;
