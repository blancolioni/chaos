private with Ada.Containers.Vectors;
private with Ada.Containers.Indefinite_Vectors;

with Chaos.Locations;

package Chaos.Features is

   type Feature_Polygon is
     array (Positive range <>) of Chaos.Locations.Pixel_Location;

   type Chaos_Feature_Record is tagged private;

   type Chaos_Feature is access all Chaos_Feature_Record'Class;

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

private

   package Polygon_Vectors is
      new Ada.Containers.Indefinite_Vectors (Positive, Feature_Polygon);

   package Polygon_Set_Vectors is
     new Ada.Containers.Vectors
       (Positive, Polygon_Vectors.Vector, Polygon_Vectors."=");

   type Feature_Type is
     (Container_Feature, Door_Feature);

   type Chaos_Feature_Record is tagged
      record
         State           : Positive := 1;
         Polygon_Sets    : Polygon_Set_Vectors.Vector;
         Sensitive_Areas : Polygon_Vectors.Vector;
      end record;

end Chaos.Features;
