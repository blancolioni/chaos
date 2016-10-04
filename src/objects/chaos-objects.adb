with Ada.Characters.Handling;

with Chaos.Localisation;
with Chaos.Logging;

with Chaos.Expressions.Classes;
with Chaos.Expressions.Environments;
with Chaos.Expressions.Text;

package body Chaos.Objects is

   function To_String
     (Object : Chaos_Object)
      return String
   is (Object.Display_Name);

   function Get_Environment
     (Object : Chaos_Object)
      return Chaos.Expressions.Chaos_Environment;

   package Chaos_Object_Expressions is
     new Chaos.Expressions.Classes
       (Class_Data_Type => Chaos_Object,
        To_String       => To_String,
        Get_Environment => Get_Environment);

   ----------------
   -- Add_Method --
   ----------------

   procedure Add_Method
     (Object         : Root_Chaos_Object_Record'Class;
      Table          : in out Chaos.Expressions.Chaos_Environment;
      Name           : String;
      Argument_Count : Natural;
      Method         : Chaos.Expressions.Primitives.Primitive_Evaluator)
   is
      pragma Unreferenced (Object);
   begin
      if Argument_Count = 0 then
         Chaos.Expressions.Insert
           (Table, Name,
            Chaos.Expressions.Primitives.Bind_Property
              (Method));
      else
         Chaos.Expressions.Insert
           (Table, Name,
            Chaos.Expressions.Primitives.Bind_Function
              (Method, Argument_Count + 1));
      end if;
   end Add_Method;

   -------------------------
   -- Create_Method_Table --
   -------------------------

   procedure Create_Method_Table
     (Object : Root_Chaos_Object_Record;
      Table  : in out Chaos.Expressions.Chaos_Environment)
   is
   begin
      Chaos.Expressions.Insert
        (Table, "identifier",
         Chaos.Expressions.Text.To_Expression (Object.Identifier));
   end Create_Method_Table;

   ------------------
   -- Display_Name --
   ------------------

   overriding function Display_Name
     (Object : Root_Localised_Object_Record)
      return String
   is
   begin
      return Chaos.Localisation.Local_Text
        (Identifier (Object));
   end Display_Name;

   ---------------------
   -- Get_Environment --
   ---------------------

   function Get_Environment
     (Object : Chaos_Object)
      return Chaos.Expressions.Chaos_Environment
   is
      Env : Chaos.Expressions.Chaos_Environment :=
              Chaos.Expressions.Environments.New_Environment;
   begin
      Object.Create_Method_Table (Env);
      return Env;
   end Get_Environment;

   ----------------
   -- Identifier --
   ----------------

   overriding function Identifier
     (Object : Root_Chaos_Object_Record)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Object.Identity);
   end Identifier;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Object   : in out Root_Chaos_Object_Record'Class;
      Identity : String)
   is
   begin
      Object.Identity :=
        Ada.Strings.Unbounded.To_Unbounded_String (Identity);
   end Initialize;

   -----------------------
   -- Local_Environment --
   -----------------------

   function Local_Environment
     (Object : access constant Root_Chaos_Object_Record'Class)
      return Chaos.Expressions.Chaos_Environment
   is
   begin
      return Get_Environment (Chaos_Object (Object));
   end Local_Environment;

   ---------
   -- Log --
   ---------

   procedure Log
     (Object  : Root_Chaos_Object_Record'Class;
      Message : String)
   is
   begin
      Chaos.Logging.Log
        (Ada.Characters.Handling.To_Upper
           (Object.Object_Database.Database_Class_Name),
         Message);
   end Log;

   -------------------
   -- To_Expression --
   -------------------

   function To_Expression
     (Object : access constant Root_Chaos_Object_Record'Class)
      return Chaos.Expressions.Chaos_Expression
   is
   begin
      if Object = null then
         return Chaos.Expressions.Null_Value;
      else
         return Chaos_Object_Expressions.To_Expression
           (Chaos_Object (Object));
      end if;
   end To_Expression;

end Chaos.Objects;
