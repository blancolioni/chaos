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
         begin
            Wed.Get (Overlay.Width);
            Wed.Get (Overlay.Height);
            Wed.Get (Overlay.Tileset_Name);
            Chaos.Logging.Log ("WED", String (Overlay.Tileset_Name));
            Wed.Get (Overlay.Unique_Tile_Count);
            Wed.Get (Overlay.Movement_Type);
            Wed.Get (Overlay.Tilemap_Offset);
            Wed.Get (Overlay.Tile_Index_Lookup);
            Wed.Overlays.Append (Overlay);
         end;
      end loop;

   end Load;

end Chaos.Resources.Wed;
