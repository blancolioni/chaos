with Ada.Containers.Vectors;

package body Chaos.Expressions.Primitives is

   package Argument_Vectors is
     new Ada.Containers.Vectors (Positive, Chaos_Expression);

   type Primitive_Expression_Record is
     new Root_Chaos_Expression_Record with
      record
         Property  : Boolean;
         Arg_Count : Natural;
         Partial   : Argument_Vectors.Vector;
         Fn        : Primitive_Evaluator;
      end record;

   overriding function To_String
     (Expression : Primitive_Expression_Record)
      return String
   is ("[primitive]");

   overriding function Is_Atom
     (Expression : Primitive_Expression_Record)
      return Boolean
   is (False);

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
      Argument    : Chaos_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression;

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (Expression  : Primitive_Expression_Record;
      Argument    : Chaos_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression
   is
   begin
      if Expression.Partial.Last_Index + 1 = Expression.Arg_Count then
         declare
            Args : Array_Of_Expressions (1 .. Expression.Arg_Count);
         begin
            for I in 1 .. Expression.Partial.Last_Index loop
               Args (I) := Expression.Partial.Element (I);
            end loop;
            Args (Args'Last) := Argument;
            return Expression.Fn (Environment, Args);
         end;
      else
         declare
            Copy : Primitive_Expression_Record'Class := Expression;
         begin
            Copy.Partial.Append (Argument);
            return Create (Copy);
         end;
      end if;
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
              (Root_Chaos_Expression_Record with
               False, Argument_Count, Argument_Vectors.Empty_Vector,
               Evaluator);
   begin
      return Express (Rec);
   end Bind_Function;

   -------------------
   -- Bind_Property --
   -------------------

   function Bind_Property
     (Evaluator      : Primitive_Evaluator)
      return Chaos_Expression
   is
      Rec : constant Primitive_Expression_Record :=
              (Root_Chaos_Expression_Record with
               True, 1, Argument_Vectors.Empty_Vector, Evaluator);
   begin
      return Express (Rec);
   end Bind_Property;

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

   -----------------
   -- Is_Property --
   -----------------

   function Is_Property
     (Expression : Chaos_Expression)
      return Boolean
   is
   begin
      return Get (Expression) in Primitive_Expression_Record'Class
        and then Primitive_Expression_Record'Class (Get (Expression)).Property;
   end Is_Property;

end Chaos.Expressions.Primitives;
