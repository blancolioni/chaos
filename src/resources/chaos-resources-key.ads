private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

private with WL.String_Maps;

package Chaos.Resources.Key is

   type Key_Resource is
     new Chaos_Resource with private;

   function Get_Resource_Location
     (Key       : Key_Resource'Class;
      Reference : Resource_Reference;
      Res_Type  : Resource_Type;
      Locator   : out WL.Binary_IO.Word_32)
      return String;

private

   type Biff_Entry is
      record
         Path    : Ada.Strings.Unbounded.Unbounded_String;
         Length  : Word_32;
         CD      : Positive;
         Cache   : Boolean;
         Data    : Boolean;
      end record;

   package Biff_Entry_Vectors is
     new Ada.Containers.Vectors (Natural, Biff_Entry);

   type Resource_Entry is
      record
         Resource_Name  : Resource_Reference;
         Resource_Type  : Word_16;
         Biff_Index     : Natural;
         Locator        : Word_32;
      end record;

   package Resource_Entry_Vectors is
      new Ada.Containers.Vectors (Positive, Resource_Entry);

   package Resource_Entry_Maps is
     new WL.String_Maps (Resource_Entry);

   type Key_Resource is
     new Chaos_Resource with
      record
         Biff_Entries     : Biff_Entry_Vectors.Vector;
         Resource_Entries : Resource_Entry_Vectors.Vector;
         Resource_Map     : Resource_Entry_Maps.Map;
      end record;

   overriding function Signature
     (Key : Key_Resource)
      return String
   is ("KEY ");

   overriding procedure Load
     (Key : in out Key_Resource);

end Chaos.Resources.Key;
