with Ada.Text_IO;

with Chaos.Expressions.Numbers;
with Chaos.Expressions.Primitives;

package body Chaos.Expressions.Environments is

   Local_Standard_Environment : Chaos_Environment;
   Local_Global_Environment   : Chaos_Environment;

   type Global_Object_Expression is
     new Constant_Chaos_Expression_Record with null record;

   overriding function To_String
     (Global : Global_Object_Expression)
      return String
   is ("GLOBAL");

   overriding function Local_Environment
     (Global : Global_Object_Expression)
      return Chaos_Environment
   is (Local_Global_Environment);

   overriding procedure Set
     (Global : in out Global_Object_Expression;
      Name   : String;
      Value  : Chaos_Expression);

   Local_Global_Object : aliased Global_Object_Expression;

   procedure Create_Standard;
   Standard_Created : Boolean := False;

   function Evaluate_Equal
     (Environment : Chaos_Environment;
      Arguments   : Array_Of_Expressions)
      return Chaos_Expression;

   function Evaluate_Add
     (Environment : Chaos_Environment;
      Arguments   : Array_Of_Expressions)
      return Chaos_Expression;

   function Evaluate_Display_String
     (Environment : Chaos_Environment;
      Arguments   : Array_Of_Expressions)
      return Chaos_Expression;

   ------------------------
   -- Add_Standard_Value --
   ------------------------

   procedure Add_Standard_Value
     (Name  : String;
      Value : Chaos_Expression)
   is
   begin
      if not Standard_Created then
         Create_Standard;
      end if;
      Insert (Local_Standard_Environment, Name, Value);
   end Add_Standard_Value;

   ---------------------
   -- Create_Standard --
   ---------------------

   procedure Create_Standard is

      procedure Fn
        (Name : String;
         Args : Natural;
         F    : Chaos.Expressions.Primitives.Primitive_Evaluator);

      --------
      -- Fn --
      --------

      procedure Fn
        (Name : String;
         Args : Natural;
         F    : Chaos.Expressions.Primitives.Primitive_Evaluator)
      is
      begin
         Insert (Local_Standard_Environment, Name,
                 Chaos.Expressions.Primitives.Bind_Function
                   (F, Args));
      end Fn;

   begin
      Local_Standard_Environment := New_Environment;
      Local_Global_Environment := New_Environment;

      Insert (Local_Standard_Environment, "global",
              Global_Object);
      Fn ("=", 2, Evaluate_Equal'Access);
      Fn ("+", 2, Evaluate_Add'Access);
      Fn ("display-string", 1, Evaluate_Display_String'Access);
      Standard_Created := True;
   end Create_Standard;

   ------------------
   -- Evaluate_Add --
   ------------------

   function Evaluate_Add
     (Environment : Chaos_Environment;
      Arguments   : Array_Of_Expressions)
      return Chaos_Expression
   is
      pragma Unreferenced (Environment);
      X : constant Integer :=
            Chaos.Expressions.Numbers.To_Integer (Arguments (1));
      Y : constant Integer :=
            Chaos.Expressions.Numbers.To_Integer (Arguments (2));
   begin
      return Chaos.Expressions.Numbers.To_Expression (X + Y);
   end Evaluate_Add;

   -----------------------------
   -- Evaluate_Display_String --
   -----------------------------

   function Evaluate_Display_String
     (Environment : Chaos_Environment;
      Arguments   : Array_Of_Expressions)
      return Chaos_Expression
   is
      pragma Unreferenced (Environment);
   begin
      Ada.Text_IO.Put (To_String (Arguments (1)));
      return Null_Value;
   end Evaluate_Display_String;

   --------------------
   -- Evaluate_Equal --
   --------------------

   function Evaluate_Equal
     (Environment : Chaos_Environment;
      Arguments   : Array_Of_Expressions)
      return Chaos_Expression
   is
      pragma Unreferenced (Environment);
   begin
      return (if Get (Arguments (1)).Equal (Get (Arguments (2)))
              then Always else Never);
   end Evaluate_Equal;

   ------------
   -- Global --
   ------------

   function Global
     (Name : String)
      return Chaos_Expression
   is
   begin
      return Find (Local_Global_Environment, Name);
   end Global;

   -------------------
   -- Global_Object --
   -------------------

   function Global_Object return Chaos_Expression is
   begin
      return Create (Local_Global_Object);
   end Global_Object;

   ---------------------
   -- New_Environment --
   ---------------------

   function New_Environment return Chaos_Environment is
   begin
      return Env : Chaos_Environment do
         Push_Table (Env);
      end return;
   end New_Environment;

   ---------
   -- Set --
   ---------

   overriding procedure Set
     (Global : in out Global_Object_Expression;
      Name   : String;
      Value  : Chaos_Expression)
   is
      pragma Unreferenced (Global);
   begin
      Set_Global (Name, Value);
   end Set;

   ----------------
   -- Set_Global --
   ----------------

   procedure Set_Global
     (Name  : String;
      Value : Chaos_Expression)
   is
   begin
      Insert (Local_Global_Environment, Name, Value);
   end Set_Global;

   --------------------------
   -- Standard_Environment --
   --------------------------

   function Standard_Environment return Chaos_Environment is
   begin
      if not Standard_Created then
         Create_Standard;
      end if;
      return Local_Standard_Environment;
   end Standard_Environment;

end Chaos.Expressions.Environments;
