with Ada.Characters.Handling;

with Chaos.Localisation;
with Chaos.Logging;

package body Chaos.Objects is

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

   ----------------
   -- Identifier --
   ----------------

   overriding function Identifier
     (Object : Root_Chaos_Object_Record)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Object.Identifier);
   end Identifier;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Object   : in out Root_Chaos_Object_Record'Class;
      Identity : String)
   is
   begin
      Object.Identifier :=
        Ada.Strings.Unbounded.To_Unbounded_String (Identity);
   end Initialize;

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

end Chaos.Objects;
