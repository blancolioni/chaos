package Chaos.Expressions.Conditional is

   function Create_Conditional
     (Condition   : Chaos_Expression;
      True_Value  : Chaos_Expression;
      False_Value : Chaos_Expression)
      return Chaos_Expression;

end Chaos.Expressions.Conditional;
