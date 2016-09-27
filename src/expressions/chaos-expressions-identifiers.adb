with Ada.Strings.Unbounded;

package body Chaos.Expressions.Identifiers is

   type Identifier_Expression_Record is
     new Constant_Chaos_Expression_Record with
      record
         Value : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function To_String
     (Expression  : Identifier_Expression_Record)
      return String
   is (Ada.Strings.Unbounded.To_String (Expression.Value));

   overriding function Evaluate
     (Expression  : Identifier_Expression_Record;
      Environment : Chaos_Environment)
      return Chaos_Expression;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Expression  : Identifier_Expression_Record;
      Environment : Chaos_Environment)
      return Chaos_Expression
   is
   begin
      return Find (Environment,
                   Ada.Strings.Unbounded.To_String (Expression.Value));
   end Evaluate;

   -------------------
   -- To_Expression --
   -------------------

   function To_Expression (Value : String) return Chaos_Expression is
      Rec : constant Identifier_Expression_Record :=
              (Constant_Chaos_Expression_Record with
               Ada.Strings.Unbounded.To_Unbounded_String (Value));
   begin
      return Express (Rec);
   end To_Expression;

end Chaos.Expressions.Identifiers;
