private with Ada.Containers.Vectors;
private with Ada.Containers.Indefinite_Vectors;

private with Memor;

private with Chaos.Resources;

with Chaos.Locations;
with Chaos.Objects;
with Chaos.Items;

package Chaos.Features is

   type Feature_Polygon is
     array (Positive range <>) of Chaos.Locations.Pixel_Location;

   type Chaos_Feature_Record is
     new Chaos.Objects.Root_Chaos_Object_Record with private;

   type Chaos_Feature is access constant Chaos_Feature_Record'Class;

   function Polygon_Count
     (Feature : Chaos_Feature_Record'Class)
      return Natural;

   function Polygon
     (Feature : Chaos_Feature_Record'Class;
      Index   : Positive)
      return Feature_Polygon
     with Pre => Index <= Feature.Polygon_Count;

   function Sensitive_Area
     (Feature : Chaos_Feature_Record'Class)
      return Feature_Polygon;

   function Bounding_Box
     (Feature : Chaos_Feature_Record'Class)
      return Chaos.Locations.Pixel_Rectangle;

   function Cursor_Index
     (Feature : Chaos_Feature_Record'Class)
      return Natural;

   function Has_Destination
     (Feature : Chaos_Feature_Record'Class)
      return Boolean;

   function Destination_Name
     (Feature : Chaos_Feature_Record'Class)
      return String;

   function Destination_Entrance_Name
     (Feature : Chaos_Feature_Record'Class)
      return String;

private

   package Polygon_Vectors is
      new Ada.Containers.Indefinite_Vectors (Positive, Feature_Polygon);

   package Polygon_Set_Vectors is
     new Ada.Containers.Vectors
       (Positive, Polygon_Vectors.Vector, Polygon_Vectors."=");

   type Feature_Type is
     (Container_Feature, Door_Feature);

   package Item_Vectors is
     new Ada.Containers.Vectors
       (Positive, Chaos.Items.Chaos_Item, Chaos.Items."=");

   type Chaos_Feature_Record is
     new Chaos.Objects.Root_Chaos_Object_Record with
      record
         State                : Positive := 1;
         Polygon_Sets         : Polygon_Set_Vectors.Vector;
         Sensitive_Areas      : Polygon_Vectors.Vector;
         Travel               : Boolean := False;
         Destination          : Chaos.Resources.Resource_Reference;
         Destination_Entrance : String (1 .. 32);
         Bounding_Box         : Chaos.Locations.Pixel_Rectangle;
         Cursor_Index         : Natural;
         Capacity             : Natural;
         Contents             : Item_Vectors.Vector;
      end record;

   overriding function Object_Database
     (Feature : Chaos_Feature_Record)
      return Memor.Memor_Database;

   overriding procedure Add_Properties
     (Feature : Chaos_Feature_Record);

end Chaos.Features;
