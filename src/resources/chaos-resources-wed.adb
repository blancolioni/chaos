with Chaos.Logging;

package body Chaos.Resources.Wed is

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
                               & Overlay.Height'Img
                               & ";"
                               & Tile_Count'Img
                               & " unique tiles");
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

   end Load;

end Chaos.Resources.Wed;
