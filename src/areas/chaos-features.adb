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
      return Feature.Polygon_Sets.Element (Feature.State).Element (Index);
   end Polygon;

   -------------------
   -- Polygon_Count --
   -------------------

   function Polygon_Count
     (Feature : Chaos_Feature_Record'Class)
      return Natural
   is
   begin
      return Feature.Polygon_Sets.Element (Feature.State).Last_Index;
   end Polygon_Count;

   --------------------
   -- Sensitive_Area --
   --------------------

   function Sensitive_Area
     (Feature : Chaos_Feature_Record'Class)
      return Feature_Polygon
   is
   begin
      return Feature.Sensitive_Areas.Element (Feature.State);
   end Sensitive_Area;

end Chaos.Features;
