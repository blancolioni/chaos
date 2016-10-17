private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

with Chaos.Resources.Text;

package Chaos.Resources.Ids is

   type Ids_Resource is
     new Chaos.Resources.Text.Text_Resource with private;

   function Row_Count
     (Ids : Ids_Resource'Class)
      return Natural;

   function Identifier
     (Ids   : Ids_Resource'Class;
      Index : Positive)
      return String;

   function Value
     (Ids     : Ids_Resource'Class;
      Index   : Positive)
      return Integer;

private

   type Identifier_Record is
      record
         Identifier : Ada.Strings.Unbounded.Unbounded_String;
         Value      : Integer;
      end record;

   package Identifier_Vectors is
     new Ada.Containers.Vectors (Positive, Identifier_Record);

   type Ids_Resource is
     new Chaos.Resources.Text.Text_Resource with
      record
         Id_Vector : Identifier_Vectors.Vector;
      end record;

   overriding procedure Load
     (Ids : in out Ids_Resource);

end Chaos.Resources.Ids;
