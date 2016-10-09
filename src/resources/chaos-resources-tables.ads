private with Ada.Containers.Indefinite_Vectors;
private with Ada.Strings.Unbounded;

with Chaos.Resources.Text;

package Chaos.Resources.Tables is

   type Table_Resource is
     new Chaos.Resources.Text.Text_Resource with private;

   function Column_Count
     (Table : Table_Resource'Class)
      return Natural;

   function Row_Count
     (Table : Table_Resource'Class)
      return Natural;

   function Column_Heading
     (Table : Table_Resource'Class;
      Index : Positive)
      return String
     with Pre => Index <= Table.Column_Count;

   function Row_Heading
     (Table : Table_Resource'Class;
      Index : Positive)
      return String
     with Pre => Index <= Table.Row_Count;

   function Get
     (Table : Table_Resource'Class;
      Row   : Positive;
      Col   : Positive)
      return String
     with Pre => Row <= Table.Row_Count and then Col <= Table.Column_Count;

   function Get
     (Table : Table_Resource'Class;
      Row   : String;
      Col   : String)
      return String;

private

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Table_Resource is
     new Chaos.Resources.Text.Text_Resource with
      record
         Row_Headings  : String_Vectors.Vector;
         Col_Headings  : String_Vectors.Vector;
         Cell_Data     : String_Vectors.Vector;
         Default_Value : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding procedure Load
     (Table : in out Table_Resource);

end Chaos.Resources.Tables;
