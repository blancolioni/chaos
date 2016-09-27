with Ada.Strings.Fixed;

package body Chaos.Expressions.Numbers is

   type Number_Expression_Record is
     new Constant_Chaos_Expression_Record with
      record
         Value : Integer;
      end record;

   overriding function To_String
     (Expression : Number_Expression_Record)
      return String
   is (Ada.Strings.Fixed.Trim (Integer'Image (Expression.Value),
                               Ada.Strings.Left));

   overriding function To_Boolean
     (Expression : Number_Expression_Record)
      return Boolean
   is (Expression.Value /= 0);

   overriding function Equal
     (Left  : Number_Expression_Record;
      Right : Root_Chaos_Expression_Record'Class)
      return Boolean;

   -----------
   -- Equal --
   -----------

   overriding function Equal
     (Left  : Number_Expression_Record;
      Right : Root_Chaos_Expression_Record'Class)
      return Boolean
   is
   begin
      if Right in Number_Expression_Record'Class then
         declare
            X : Number_Expression_Record'Class renames
                  Number_Expression_Record'Class (Right);
         begin
            return Left.Value = X.Value;
         end;
      else
         return Left.To_String = Right.To_String;
      end if;
   end Equal;

   ---------------
   -- Is_Number --
   ---------------

   function Is_Number (Expression : Chaos_Expression) return Boolean is
   begin
      return Get (Expression) in Number_Expression_Record'Class;
   end Is_Number;

   -------------------
   -- To_Expression --
   -------------------

   function To_Expression (Value : Integer) return Chaos_Expression is
      Rec : constant Number_Expression_Record :=
              (Constant_Chaos_Expression_Record with Value);
   begin
      return Express (Rec);
   end To_Expression;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer
     (Expression : Chaos_Expression)
      return Integer
   is
   begin
      if Is_Number (Expression) then
         return Number_Expression_Record (Get (Expression)).Value;
      else
         return Integer'Value (To_String (Expression));
      end if;
   end To_Integer;

end Chaos.Expressions.Numbers;
