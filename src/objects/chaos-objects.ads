private with Ada.Strings.Unbounded;
private with Chaos.Expressions.Primitives;

with Memor;

with Chaos.Expressions;

package Chaos.Objects is

   type Root_Chaos_Object_Record is
     abstract limited new Memor.Root_Record_Type
       and Memor.Identifier_Record_Type
   with private;

   type Chaos_Object is access constant Root_Chaos_Object_Record'Class;

   overriding function Identifier
     (Object : Root_Chaos_Object_Record)
      return String;

   procedure Initialize
     (Object   : in out Root_Chaos_Object_Record'Class;
      Identity : String);

   function Display_Name
     (Object : Root_Chaos_Object_Record)
      return String
   is (Object.Identifier);

   function To_Expression
     (Object : access constant Root_Chaos_Object_Record'Class)
      return Chaos.Expressions.Chaos_Expression;

   function Local_Environment
     (Object : access constant Root_Chaos_Object_Record'Class)
      return Chaos.Expressions.Chaos_Environment;

   procedure Log
     (Object  : Root_Chaos_Object_Record'Class;
      Message : String);

   procedure Create_Method_Table
     (Object : Root_Chaos_Object_Record;
      Table  : in out Chaos.Expressions.Chaos_Environment);

   type Root_Localised_Object_Record is
     abstract limited new Root_Chaos_Object_Record with private;

   overriding function Display_Name
     (Object : Root_Localised_Object_Record)
      return String;

private

   type Root_Chaos_Object_Record is
     abstract limited new Memor.Root_Record_Type
     and Memor.Identifier_Record_Type with
      record
         Identity : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   procedure Add_Method
     (Object         : Root_Chaos_Object_Record'Class;
      Table          : in out Chaos.Expressions.Chaos_Environment;
      Name           : String;
      Argument_Count : Natural;
      Method         : Chaos.Expressions.Primitives.Primitive_Evaluator);

   type Root_Localised_Object_Record is
     abstract limited new Root_Chaos_Object_Record with null record;

end Chaos.Objects;
