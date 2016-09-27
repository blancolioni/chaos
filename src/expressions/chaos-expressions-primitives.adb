package body Chaos.Expressions.Primitives is

   type Primitive_Expression_Record is
     new Root_Chaos_Expression_Record with
      record
         Arg_Count : Natural;
         Fn        : Primitive_Evaluator;
      end record;

   overriding function To_String
     (Expression : Primitive_Expression_Record)
      return String
   is ("[primitive]");

   overriding function To_Boolean
     (Expression : Primitive_Expression_Record)
      return Boolean
   is (True);

   overriding function Evaluate
     (Expression  : Primitive_Expression_Record;
      Environment : Chaos_Environment)
      return Chaos_Expression;

   overriding function Apply
     (Expression  : Primitive_Expression_Record;
      Environment : Chaos_Environment;
      Arguments   : Array_Of_Expressions)
      return Chaos_Expression;

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (Expression  : Primitive_Expression_Record;
      Environment : Chaos_Environment;
      Arguments   : Array_Of_Expressions)
      return Chaos_Expression
   is
      Actual_Args : Array_Of_Expressions (1 .. Expression.Arg_Count);
   begin
      for I in Actual_Args'Range loop
         if I in Arguments'Range then
            Actual_Args (I) := Arguments (I);
         else
            Actual_Args (I) := Null_Value;
         end if;
      end loop;
      return Expression.Fn (Environment, Actual_Args);
   end Apply;

   -------------------
   -- Bind_Function --
   -------------------

   function Bind_Function
     (Evaluator      : Primitive_Evaluator;
      Argument_Count : Natural)
      return Chaos_Expression
   is
      Rec : constant Primitive_Expression_Record :=
              (Root_Chaos_Expression_Record with Argument_Count, Evaluator);
   begin
      return Express (Rec);
   end Bind_Function;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Expression  : Primitive_Expression_Record;
      Environment : Chaos_Environment)
      return Chaos_Expression
   is
   begin
      if Expression.Arg_Count = 0 then
         return Expression.Fn (Environment, No_Array);
      else
         return Create (Expression);
      end if;
   end Evaluate;

end Chaos.Expressions.Primitives;
