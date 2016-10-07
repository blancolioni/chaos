with Chaos.Logging;

package body Chaos.Resources.Wed is

   procedure Read_Polygons
     (Wed      : in out Wed_Resource'Class;
      Offset   : Word_32;
      Count    : Word_16;
      Polygons : in out Polygon_Entry_Vectors.Vector);

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Wed : in out Wed_Resource)
   is
   begin
      Wed.Set_Offset (8);
      Wed.Get (Wed.Overlay_Count);
      Wed.Get (Wed.Door_Count);
      Wed.Get (Wed.Overlay_Offset);
      Wed.Get (Wed.Secondary_Header_Offset);
      Wed.Get (Wed.Doors_Offset);
      Wed.Get (Wed.Door_Tile_Cell_Offset);

      Wed.Push_Offset (Wed.Secondary_Header_Offset);
      Wed.Get (Wed.Wall_Polygon_Count);
      Wed.Get (Wed.Polygons_Offset);
      Wed.Get (Wed.Vertices_Offset);
      Wed.Get (Wed.Wall_Groups_Offset);
      Wed.Get (Wed.Polygon_Indices_Offset);
      Wed.Pop_Offset;

      for I in 1 .. Wed.Overlay_Count loop
         declare
            Overlay : Overlay_Entry;
            Tile_Count : Word_16 := 0;
         begin
            Wed.Get (Overlay.Width);
            Wed.Get (Overlay.Height);
            Wed.Get (Overlay.Tileset_Name);
            Wed.Get (Overlay.Unique_Tile_Count);
            Wed.Get (Overlay.Movement_Type);
            Wed.Get (Overlay.Tilemap_Offset);
            Wed.Get (Overlay.Tile_Index_Lookup);

            Wed.Push_Offset (Overlay.Tilemap_Offset);
            for J in 1 .. Overlay.Width * Overlay.Height loop
               declare
                  Map_Entry    : Tile_Map_Entry;
                  Finish_Index : Word_16;
               begin
                  Wed.Get (Map_Entry.Start_Index);
                  Wed.Get (Map_Entry.Tile_Count);
                  Wed.Get (Map_Entry.Secondary_Index);
                  Wed.Get (Map_Entry.Overlay_Flags);
                  Wed.Skip (3);
                  Finish_Index :=
                    Map_Entry.Start_Index + Map_Entry.Tile_Count - 1;
                  Overlay.Tile_Map.Append (Map_Entry);

                  if Map_Entry.Tile_Count > 0 then
                     Tile_Count :=
                       Word_16'Max (Tile_Count, Finish_Index + 1);
                  end if;
                  if Map_Entry.Secondary_Index /= 16#FFFF# then
                     Tile_Count :=
                       Word_16'Max (Tile_Count,
                                    Map_Entry.Secondary_Index + 1);
                  end if;
               end;
            end loop;
            Wed.Pop_Offset;

            Chaos.Logging.Log ("WED",
                               String (Overlay.Tileset_Name)
                               & Overlay.Width'Img
                               & " x"
                               & Overlay.Height'Img);

            Wed.Push_Offset (Overlay.Tile_Index_Lookup);
            for J in 1 .. Natural (Tile_Count) loop
               declare
                  X : Word_16;
               begin
                  Wed.Get (X);
                  Overlay.Tile_Indices.Append (X);
               end;
            end loop;
            Wed.Pop_Offset;

            Wed.Overlays.Append (Overlay);
         end;
      end loop;

      Wed.Push_Offset (Wed.Doors_Offset);
      for I in 1 .. Wed.Door_Count loop
         declare
            Door : Door_Entry;
         begin
            Wed.Get (Door.Name);
            Wed.Get (Door.Closed);
            Wed.Get (Door.First_Tile_Cell_Index);
            Wed.Get (Door.Tile_Cell_Count);
            Wed.Get (Door.Polygon_Count_Open);
            Wed.Get (Door.Polygon_Count_Closed);
            Wed.Get (Door.Polygon_Offset_Open);
            Wed.Get (Door.Polygon_Offset_Closed);
            Read_Polygons (Wed, Door.Polygon_Offset_Open,
                           Door.Polygon_Count_Open,
                           Door.Polygons_Open);
            Read_Polygons (Wed, Door.Polygon_Offset_Closed,
                           Door.Polygon_Count_Closed,
                           Door.Polygons_Closed);

            Wed.Doors.Append (Door);
         end;
      end loop;

      Wed.Pop_Offset;

      Wed.Push_Offset (Wed.Vertices_Offset);
      for I in 1 .. Wed.Vertex_Count loop
         declare
            V : Vertex;
         begin
            Wed.Get (V.X);
            Wed.Get (V.Y);
            Wed.Vertices.Append (V);
         end;
      end loop;

      Chaos.Logging.Log
        ("WED",
         Wed.Door_Count'Img
         & " doors and"
         & Wed.Vertex_Count'Img
         & " vertices");

   end Load;

   -------------------
   -- Read_Polygons --
   -------------------

   procedure Read_Polygons
     (Wed      : in out Wed_Resource'Class;
      Offset   : Word_32;
      Count    : Word_16;
      Polygons : in out Polygon_Entry_Vectors.Vector)
   is
   begin
      Wed.Push_Offset (Offset);
      for I in 1 .. Count loop
         declare
            Polygon : Polygon_Entry;
         begin
            Wed.Get (Polygon.Start_Vertex_Index);
            Wed.Get (Polygon.Vertex_Count);
            Wed.Get (Polygon.Flags);
            Wed.Get (Polygon.Height);
            Wed.Get (Polygon.Min_X);
            Wed.Get (Polygon.Max_X);
            Wed.Get (Polygon.Min_Y);
            Wed.Get (Polygon.Max_Y);
            Wed.Vertex_Count :=
              Natural'Max (Wed.Vertex_Count,
                           Natural
                             (Polygon.Start_Vertex_Index
                              + Polygon.Vertex_Count));
            Polygons.Append (Polygon);
         end;
      end loop;
      Wed.Pop_Offset;
   end Read_Polygons;

end Chaos.Resources.Wed;
