package body Chaos.Expressions.Classes is

   type Class_Expression_Record is
     new Constant_Chaos_Expression_Record with
      record
         Value : Class_Data_Type;
      end record;

   overriding function Local_Environment
     (Expression  : Class_Expression_Record)
      return Chaos_Environment;

   overriding function To_String
     (Expression  : Class_Expression_Record)
      return String
   is (To_String (Expression.Value));

   ---------------
   -- Is_Class --
   ---------------

   function Is_Class (Expression : Chaos_Expression) return Boolean is
   begin
      return Get (Expression) in Class_Expression_Record'Class;
   end Is_Class;

   -----------------------
   -- Local_Environment --
   -----------------------

   overriding function Local_Environment
     (Expression  : Class_Expression_Record)
      return Chaos_Environment
   is
   begin
      return Get_Environment (Expression.Value);
   end Local_Environment;

   ---------------
   -- To_Class --
   ---------------

   function To_Class
     (Expression : Chaos_Expression)
      return Class_Data_Type
   is
      pragma Assert (Is_Class (Expression));
   begin
      return Class_Expression_Record (Get (Expression)).Value;
   end To_Class;

   -------------------
   -- To_Expression --
   -------------------

   function To_Expression
     (Value : Class_Data_Type)
      return Chaos_Expression
   is
      Rec : constant Class_Expression_Record :=
              (Constant_Chaos_Expression_Record with Value);
   begin
      return Express (Rec);
   end To_Expression;

   -------------------
   -- Update_Class --
   -------------------

   procedure Update_Class
     (Expression : Chaos_Expression;
      Update     : not null access
        procedure (Value : in out Class_Data_Type))
   is
   begin
      Update (Class_Expression_Record (Set (Expression).all).Value);
   end Update_Class;

end Chaos.Expressions.Classes;
