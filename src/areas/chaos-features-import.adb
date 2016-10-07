package body Chaos.Features.Import is

   -----------------
   -- Import_Door --
   -----------------

   function Import_Door
     (Wed   : Chaos.Resources.Wed.Wed_Resource'Class;
      Index : Positive)
      return Chaos_Feature
   is
      Feature : constant Chaos_Feature := new Chaos_Feature_Record;
      Door    : Chaos.Resources.Wed.Door_Entry renames
                  Wed.Doors.Element (Index);
   begin
      for Polygon of Door.Polygons_Open loop
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
            Feature.Polygons.Append (Feature_Poly);
         end;
      end loop;

      return Feature;

   end Import_Door;

end Chaos.Features.Import;
