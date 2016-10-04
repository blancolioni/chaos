with Ada.Characters.Handling;

package body Chaos.Expressions.Enumerated is

   type Enum_Expression_Record is
     new Constant_Chaos_Expression_Record with
      record
         Value : Enum;
      end record;

   overriding function To_String
     (Expression  : Enum_Expression_Record)
      return String;

   ------------------------
   -- Add_To_Environment --
   ------------------------

   procedure Add_To_Environment
     (Target : in out Chaos_Environment)
   is
   begin
      for Value in Enum loop
         declare
            E : constant Chaos_Expression := To_Expression (Value);
         begin
            Insert (Target, To_String (E), E);
         end;
      end loop;
   end Add_To_Environment;

   -------------
   -- Is_Enum --
   -------------

   function Is_Enum (Expression : Chaos_Expression) return Boolean is
   begin
      return Get (Expression) in Enum_Expression_Record'Class;
   end Is_Enum;

   -------------
   -- To_Enum --
   -------------

   function To_Enum (Expression : Chaos_Expression) return Enum is
   begin
      if Is_Enum (Expression) then
         return Enum_Expression_Record (Get (Expression)).Value;
      else
         declare
            Image : String := To_String (Expression);
         begin
            for I in Image'Range loop
               if Image (I) = '-' then
                  Image (I) := '_';
               end if;
            end loop;
            return Enum'Value (Image);
         end;
      end if;
   end To_Enum;

   -------------------
   -- To_Expression --
   -------------------

   function To_Expression (Value : Enum) return Chaos_Expression is
      Rec : constant Enum_Expression_Record :=
              (Constant_Chaos_Expression_Record with Value);
   begin
      return Express (Rec);
   end To_Expression;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Expression  : Enum_Expression_Record)
      return String
   is
      Result : String :=
                 Ada.Characters.Handling.To_Lower
                   (Enum'Image (Expression.Value));
   begin
      for I in Result'Range loop
         if Result (I) = '_' then
            Result (I) := '-';
         end if;
      end loop;
      return Result;
   end To_String;

end Chaos.Expressions.Enumerated;
