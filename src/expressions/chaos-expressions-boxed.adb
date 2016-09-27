package body Chaos.Expressions.Boxed is

   type Boxed_Expression_Record is
     new Constant_Chaos_Expression_Record with
      record
         Value : Value_Type;
      end record;

   overriding function To_String
     (Expression  : Boxed_Expression_Record)
      return String
   is (To_String (Expression.Value));

   ------------
   -- Is_Box --
   ------------

   function Is_Box (Expression : Chaos_Expression) return Boolean is
   begin
      return Get (Expression) in Boxed_Expression_Record'Class;
   end Is_Box;

   -------------------
   -- To_Expression --
   -------------------

   function To_Expression (Value : Value_Type) return Chaos_Expression is
      Rec : constant Boxed_Expression_Record :=
              (Constant_Chaos_Expression_Record with Value);
   begin
      return Express (Rec);
   end To_Expression;

end Chaos.Expressions.Boxed;
