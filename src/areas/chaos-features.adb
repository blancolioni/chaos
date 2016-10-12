package body Chaos.Features is

   ------------------
   -- Bounding_Box --
   ------------------

   function Bounding_Box
     (Feature : Chaos_Feature_Record'Class)
      return Chaos.Locations.Pixel_Rectangle
   is
   begin
      return Feature.Bounding_Box;
   end Bounding_Box;

   ------------------
   -- Cursor_Index --
   ------------------

   function Cursor_Index
     (Feature : Chaos_Feature_Record'Class)
      return Natural
   is
   begin
      return Feature.Cursor_Index;
   end Cursor_Index;

   -------------------------------
   -- Destination_Entrance_Name --
   -------------------------------

   function Destination_Entrance_Name
     (Feature : Chaos_Feature_Record'Class)
      return String
   is
   begin
      return Feature.Destination_Entrance;
   end Destination_Entrance_Name;

   ----------------------
   -- Destination_Name --
   ----------------------

   function Destination_Name
     (Feature : Chaos_Feature_Record'Class)
      return String
   is
   begin
      return Chaos.Resources.To_String (Feature.Destination);
   end Destination_Name;

   ---------------------
   -- Has_Destination --
   ---------------------

   function Has_Destination
     (Feature : Chaos_Feature_Record'Class)
      return Boolean
   is
   begin
      return Feature.Travel;
   end Has_Destination;

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
