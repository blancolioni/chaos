package body Chaos.Expressions.Objects is

   Object_VT : Chaos_Environment;
   Got_VT    : Boolean := False;

   type Object_Expression_Record is
     new Constant_Chaos_Expression_Record with
      record
         Value : Object_Data_Type;
      end record;

   overriding function Local_Environment
     (Expression  : Object_Expression_Record)
      return Chaos_Environment;

   overriding function To_String
     (Expression  : Object_Expression_Record)
      return String
   is (To_String (Expression.Value));

   procedure Create_VT;

   ---------------
   -- Create_VT --
   ---------------

   procedure Create_VT is
   begin
      Push_Table (Object_VT);
      Create_Virtual_Table (Object_VT);
      Got_VT := True;
   end Create_VT;

   ---------------
   -- Is_Object --
   ---------------

   function Is_Object (Expression : Chaos_Expression) return Boolean is
   begin
      return Get (Expression) in Object_Expression_Record'Class;
   end Is_Object;

   -----------------------
   -- Local_Environment --
   -----------------------

   overriding function Local_Environment
     (Expression  : Object_Expression_Record)
      return Chaos_Environment
   is
      pragma Unreferenced (Expression);
   begin
      if not Got_VT then
         Create_VT;
      end if;
      return Object_VT;
   end Local_Environment;

   -------------------
   -- To_Expression --
   -------------------

   function To_Expression
     (Value : Object_Data_Type)
      return Chaos_Expression
   is
      Rec : constant Object_Expression_Record :=
              (Constant_Chaos_Expression_Record with Value);
   begin
      if not Got_VT then
         Create_VT;
      end if;

      return Express (Rec);
   end To_Expression;

   ---------------
   -- To_Object --
   ---------------

   function To_Object
     (Expression : Chaos_Expression)
      return Object_Data_Type
   is
      pragma Assert (Is_Object (Expression));
   begin
      return Object_Expression_Record (Get (Expression)).Value;
   end To_Object;

   -------------------
   -- Update_Object --
   -------------------

   procedure Update_Object
     (Expression : Chaos_Expression;
      Update     : not null access
        procedure (Value : in out Object_Data_Type))
   is
   begin
      Update (Object_Expression_Record (Set (Expression).all).Value);
   end Update_Object;

end Chaos.Expressions.Objects;
