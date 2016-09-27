private with Ada.Strings.Unbounded;

with Memor;

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
         Identifier : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   type Root_Localised_Object_Record is
     abstract limited new Root_Chaos_Object_Record with null record;

end Chaos.Objects;
