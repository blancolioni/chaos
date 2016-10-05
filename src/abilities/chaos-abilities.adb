with Chaos.Expressions.Numbers;

package body Chaos.Abilities is

   -----------------------
   -- Ability_Modifiers --
   -----------------------

   function Ability_Modifiers
     (Able : Ability_Interface'Class)
      return Chaos.Expressions.Chaos_Environment
   is
   begin
      return Env : Chaos.Expressions.Chaos_Environment :=
        Chaos.Expressions.New_Environment
      do
         for A in Ability loop
            Chaos.Expressions.Insert
              (Env, Ability'Image (A),
               Chaos.Expressions.Numbers.To_Expression
                 (Integer (Able.Ability_Bonus (A))));
         end loop;
      end return;
   end Ability_Modifiers;

   -----------
   -- Apply --
   -----------

   procedure Apply
     (Score  : in out Ability_Score_Range;
      Change : Ability_Score_Change)
   is
      New_Score : constant Integer := Integer (Score) + Integer (Change);
   begin
      if New_Score < Integer (Ability_Score_Range'First) then
         Score := Ability_Score_Range'First;
      elsif New_Score > Integer (Ability_Score_Range'Last) then
         Score := Ability_Score_Range'Last;
      else
         Score := Ability_Score_Range (New_Score);
      end if;
   end Apply;

   ----------------------
   -- Insert_Abilities --
   ----------------------

   procedure Insert_Abilities
     (Able : Ability_Interface'Class;
      Env  : in out Chaos.Expressions.Chaos_Environment)
   is
   begin
      for A in Ability loop
         Chaos.Expressions.Insert
           (Env, Ability'Image (A),
            Chaos.Expressions.Numbers.To_Expression
              (Integer (Able.Ability_Score (A))));
         Chaos.Expressions.Insert
           (Env, Ability'Image (A) & "-mod",
            Chaos.Expressions.Numbers.To_Expression
              (Integer (Able.Ability_Bonus (A))));
      end loop;
   end Insert_Abilities;

end Chaos.Abilities;
