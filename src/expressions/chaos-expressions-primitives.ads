package Chaos.Expressions.Primitives is

   type Primitive_Evaluator is access
     function (Environment : Chaos_Environment;
               Arguments   : Array_Of_Expressions)
               return Chaos_Expression;

   function Bind_Function
     (Evaluator      : Primitive_Evaluator;
      Argument_Count : Natural)
      return Chaos_Expression;

   function Bind_Property
     (Evaluator : Primitive_Evaluator)
      return Chaos_Expression;

   function Is_Property
     (Expression : Chaos_Expression)
      return Boolean;

end Chaos.Expressions.Primitives;
