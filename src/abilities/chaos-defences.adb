with Chaos.Expressions.Numbers;

package body Chaos.Defences is

   ---------------------
   -- Insert_Defences --
   ---------------------

   procedure Insert_Defences
     (Defender : Defence_Interface'Class;
      Env      : in out Chaos.Expressions.Chaos_Environment)
   is
   begin
      for D in Defence loop
         Chaos.Expressions.Insert
           (Env, Defence'Image (D),
            Chaos.Expressions.Numbers.To_Expression
              (Integer (Defender.Defence_Score (D))));
      end loop;
   end Insert_Defences;

end Chaos.Defences;
