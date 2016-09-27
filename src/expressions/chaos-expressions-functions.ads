package Chaos.Expressions.Functions is

   function Create_Function_Call
     (Function_Name : String;
      Arguments     : Array_Of_Expressions)
      return Chaos_Expression;

   function Create_Method_Call
     (Object    : Chaos_Expression;
      Method    : String;
      Arguments : Array_Of_Expressions)
      return Chaos_Expression;

   function Create_Assignment
     (Object    : Chaos_Expression;
      Name      : String;
      Value     : Chaos_Expression)
      return Chaos_Expression;

end Chaos.Expressions.Functions;
