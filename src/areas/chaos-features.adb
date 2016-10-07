package body Chaos.Features is

   -------------
   -- Polygon --
   -------------

   function Polygon
     (Feature : Chaos_Feature_Record'Class;
      Index   : Positive)
      return Feature_Polygon
   is
   begin
      return Feature.Polygons.Element (Index);
   end Polygon;

   -------------------
   -- Polygon_Count --
   -------------------

   function Polygon_Count
     (Feature : Chaos_Feature_Record'Class)
      return Natural
   is
   begin
      return Feature.Polygons.Last_Index;
   end Polygon_Count;

end Chaos.Features;
