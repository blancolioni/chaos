package body Chaos.Features.Import is

   -----------------
   -- Import_Door --
   -----------------

   function Import_Door
     (Area       : Chaos.Resources.Area.Area_Resource'Class;
      Wed        : Chaos.Resources.Wed.Wed_Resource'Class;
      Area_Index : Positive;
      Wed_Index  : Positive)
      return Chaos_Feature
   is
      Feature   : constant Chaos_Feature := new Chaos_Feature_Record;
      Area_Door : Chaos.Resources.Area.Door_Entry renames
                    Area.Doors.Element (Area_Index);
      Wed_Door  : Chaos.Resources.Wed.Door_Entry renames
                    Wed.Doors.Element (Wed_Index);
      Open_Polygons : Polygon_Vectors.Vector;

   begin

      for Polygon of Wed_Door.Polygons_Open loop
         declare
            Vertex_Count : constant Natural :=
                             Natural (Polygon.Vertex_Count);
            Feature_Poly : Feature_Polygon  (1 .. Vertex_Count);
            Boundary_Index : Natural := 0;
         begin

            --  vertices are stpred by the resource in clockwise order,
            --  but we insist on anticlockwise boundaries
            for I in 1 .. Natural (Polygon.Vertex_Count) loop
               declare
                  V : constant Chaos.Resources.Wed.Vertex :=
                        Wed.Vertices
                          (Natural (Polygon.Start_Vertex_Index) + I);
               begin
                  Boundary_Index := Boundary_Index + 1;
                  Feature_Poly (Boundary_Index) :=
                    (Natural (V.X), Natural (V.Y));
               end;
            end loop;
            Open_Polygons.Append (Feature_Poly);
         end;
      end loop;

      Feature.Polygon_Sets.Append (Open_Polygons);

      declare
         Count : constant Natural := Natural (Area_Door.Open_Vertex_Count);
         Sensitive : Feature_Polygon (1 .. Count);
      begin
         for Index in 1 .. Count loop
            declare
               V : constant Chaos.Resources.Area.Vertex :=
                     Area.Vertices.Element
                       (Natural (Area_Door.First_Open_Vertex)
                        + Index);
            begin
               Sensitive (Index) := (Natural (V.X), Natural (V.Y));
            end;
         end loop;
         Feature.Sensitive_Areas.Append (Sensitive);
      end;

      Feature.State := 1;
      Feature.Cursor_Index := 32;

      return Feature;

   end Import_Door;

   -------------------
   -- Import_Region --
   -------------------

   function Import_Region
     (Area         : Chaos.Resources.Area.Area_Resource'Class;
      Region_Index : Positive)
      return Chaos_Feature
   is
      Region  : Chaos.Resources.Area.Region_Entry renames
                  Area.Regions (Region_Index);
      Feature : Chaos_Feature_Record;
      Vertex_Count   : constant Natural := Natural (Region.Vertex_Count);
      Feature_Poly   : Feature_Polygon  (1 .. Vertex_Count);
   begin
      for I in 1 .. Vertex_Count loop
         declare
            V : constant Chaos.Resources.Area.Vertex :=
                  Area.Vertices
                    (Natural (Region.First_Vertex) + I);
         begin
            Feature_Poly (I) :=
              (Natural (V.X), Natural (V.Y));
         end;
      end loop;

      declare
         Poly_Vec : Polygon_Vectors.Vector;
      begin
         Poly_Vec.Append (Feature_Poly);
         Feature.Polygon_Sets.Append (Poly_Vec);
      end;

      if Natural (Region.Region_Type) = 2 then
         Feature.Travel := True;
         Feature.Destination := Region.Destination_Area;
         Feature.Destination_Entrance := Region.Destination_Entrance;
      end if;

      Feature.Bounding_Box :=
        (X1 => Integer (Region.Bounding_Box.X1),
         X2 => Integer (Region.Bounding_Box.X2),
         Y1 => Integer (Region.Bounding_Box.Y1),
         Y2 => Integer (Region.Bounding_Box.Y2));

      Feature.Cursor_Index :=
        Natural (Region.Cursor_Index) + 1;

      return new Chaos_Feature_Record'(Feature);
   end Import_Region;

end Chaos.Features.Import;
