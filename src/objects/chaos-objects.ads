private with Ada.Strings.Unbounded;

with Memor;

with Lith.Objects;

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

   function Global_Setting_Name
     (Object  : Root_Chaos_Object_Record'Class;
      Setting : String)
      return String
   is (Object.Object_Database.Database_Class_Name
       & "-" & Object.Identifier
       & "-" & Setting);

   procedure Mark
     (Object : in out Root_Chaos_Object_Record;
      Mark_Value : not null access
        procedure (Value : in out Lith.Objects.Object))
   is null;

   function To_Expression
     (Object : Root_Chaos_Object_Record'Class)
      return Lith.Objects.Object;

   function Is_Object
     (Value : Lith.Objects.Object)
      return Boolean;

   function To_Object
     (Value : Lith.Objects.Object)
      return Chaos_Object
     with Pre => Is_Object (Value);

   procedure Log
     (Object  : Root_Chaos_Object_Record'Class;
      Message : String);

   procedure Add_Properties
     (Object : Root_Chaos_Object_Record)
   is abstract;

   type Property_Get_Function is access
     function (Object : Root_Chaos_Object_Record'Class)
               return Lith.Objects.Object;

   procedure Add_Property
     (Object         : Root_Chaos_Object_Record'Class;
      Name           : String;
      Get            : Property_Get_Function);

   function Property
     (Object : Root_Chaos_Object_Record'Class;
      Name   : String)
      return Lith.Objects.Object;

   procedure Save_Object
     (Object : not null access constant Root_Chaos_Object_Record'Class);

   procedure Define_Object
     (Object : Root_Chaos_Object_Record'Class);

   procedure Execute_Script
     (Object : Root_Chaos_Object_Record'Class;
      Script : Lith.Objects.Object);

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
         Identity        : Ada.Strings.Unbounded.Unbounded_String;
         Script_Executed : Boolean := False;
      end record;

   type Object_Record_Interface is
     new Lith.Objects.External_Object_Interface with
      record
         Db        : Memor.Memor_Database;
         Reference : Memor.Database_Reference;
      end record;

   overriding function Name
     (Item : Object_Record_Interface)
      return String;

   overriding function Print
     (Item  : Object_Record_Interface;
      Store : in out Lith.Objects.Object_Store'Class)
      return String;

   overriding function Equal
     (X, Y  : Object_Record_Interface;
      Store : Lith.Objects.Object_Store'Class)
      return Boolean;

   overriding procedure Mark
     (Item  : in out Object_Record_Interface;
      Store : in out Lith.Objects.Object_Store'Class;
      Mark  : not null access
        procedure (X : in out Lith.Objects.Object));

   overriding procedure Finalize
     (Item  : in out Object_Record_Interface;
      Store : in out Lith.Objects.Object_Store'Class)
   is null;

   type Root_Localised_Object_Record is
     abstract limited new Root_Chaos_Object_Record with null record;

end Chaos.Objects;
