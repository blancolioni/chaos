package body Chaos.Expressions.Conditional is

   type Conditional_Expression is
     new Root_Chaos_Expression_Record with
      record
         Condition  : Chaos_Expression;
         True_Part  : Chaos_Expression;
         False_Part : Chaos_Expression;
      end record;

   overriding function Evaluate
     (Expression  : Conditional_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression;

   overriding function Apply
     (Expression  : Conditional_Expression;
      Environment : Chaos_Environment;
      Arguments   : Array_Of_Expressions)
      return Chaos_Expression;

   overriding function To_Boolean
     (Expression  : Conditional_Expression)
      return Boolean
   is (False);

   overriding function To_String
     (Expression  : Conditional_Expression)
      return String
   is ("if " & To_String (Expression.Condition)
       & " then " & To_String (Expression.True_Part)
       & " else " & To_String (Expression.False_Part));

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (Expression  : Conditional_Expression;
      Environment : Chaos_Environment;
      Arguments   : Array_Of_Expressions)
      return Chaos_Expression
   is
   begin
      if To_Boolean (Evaluate (Environment, Expression.Condition)) then
         return Get (Expression.True_Part).Apply (Environment, Arguments);
      else
         return Get (Expression.False_Part).Apply (Environment, Arguments);
      end if;
   end Apply;

   ------------------------
   -- Create_Conditional --
   ------------------------

   function Create_Conditional
     (Condition   : Chaos_Expression;
      True_Value  : Chaos_Expression;
      False_Value : Chaos_Expression)
      return Chaos_Expression
   is
      Rec : Conditional_Expression;
   begin
      Rec.Condition := Condition;
      Rec.True_Part := True_Value;
      Rec.False_Part := False_Value;
      return Create (Rec);
   end Create_Conditional;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Expression  : Conditional_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression
   is
   begin
      if To_Boolean (Evaluate (Environment, Expression.Condition)) then
         return Evaluate (Environment, Expression.True_Part);
      else
         return Evaluate (Environment, Expression.False_Part);
      end if;
   end Evaluate;

end Chaos.Expressions.Conditional;
