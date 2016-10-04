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
