package Chaos.Expressions.Functions is

   function Apply
     (Expression : Chaos_Expression;
      Argument   : Chaos_Expression)
      return Chaos_Expression;

   function Object_Method
     (Object    : Chaos_Expression;
      Method    : String)
      return Chaos_Expression;

   function Assign
     (Object    : Chaos_Expression;
      Name      : String;
      Value     : Chaos_Expression)
      return Chaos_Expression;

   function Lambda
     (Argument    : String;
      Lambda_Body : Chaos_Expression)
      return Chaos_Expression;

end Chaos.Expressions.Functions;
