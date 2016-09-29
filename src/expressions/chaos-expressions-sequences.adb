with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package body Chaos.Expressions.Sequences is

   package Expression_Vectors is
     new Ada.Containers.Vectors (Positive, Chaos_Expression);

   function To_String (Vector : Expression_Vectors.Vector) return String;

   type Sequence_Expression_Record is
     new Root_Chaos_Expression_Record with
      record
         Items : Expression_Vectors.Vector;
      end record;

   overriding function Evaluate
     (Expression  : Sequence_Expression_Record;
      Environment : Chaos_Environment)
      return Chaos_Expression;

   overriding function Apply
     (Expression  : Sequence_Expression_Record;
      Argument    : Chaos_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression
   is (raise Constraint_Error with "can't apply a sequence");

   overriding function To_Boolean
     (Expression  : Sequence_Expression_Record)
      return Boolean
   is (True);

   overriding function To_String
     (Expression  : Sequence_Expression_Record)
      return String
   is (To_String (Expression.Items));

   ------------
   -- Append --
   ------------

   procedure Append
     (To_Sequence : Chaos_Expression;
      Value       : Chaos_Expression)
   is
   begin
      Sequence_Expression_Record'Class (Set (To_Sequence).all).Items.Append
        (Value);
   end Append;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Expression  : Sequence_Expression_Record;
      Environment : Chaos_Environment)
      return Chaos_Expression
   is
      Result : Chaos_Expression := Null_Value;
   begin
      for Expr of Expression.Items loop
         Result := Evaluate (Expr, Environment);
      end loop;
      return Result;
   end Evaluate;

   -------------------------
   -- Sequence_Expression --
   -------------------------

   function Sequence_Expression return Chaos_Expression is
      Expression : Sequence_Expression_Record;
   begin
      return Create (Expression);
   end Sequence_Expression;

   ---------------
   -- To_String --
   ---------------

   function To_String (Vector : Expression_Vectors.Vector) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for I in 1 .. Vector.Last_Index loop
         if Result /= Null_Unbounded_String then
            Result := Result & ";";
         end if;
         Result := Result & To_String (Vector.Element (I));
      end loop;
      return "<" & To_String (Result) & ">";
   end To_String;

end Chaos.Expressions.Sequences;
