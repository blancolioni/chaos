package body Chaos.Resources.Tis is

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Tis : in out Tis_Resource)
   is
   begin
      if not Tis.Tiles.Is_Empty then
         return;
      end if;

      if Tis.Read_Header then
         Tis.Set_Offset (8);
         Tis.Get (Tis.Tile_Count);
         Tis.Get (Tis.Tile_Length);
         Tis.Get (Tis.Tile_Offset);
         Tis.Get (Tis.Tile_Pixel_Count);
         Tis.Set_Offset (Tis.Tile_Offset);
      end if;

      for Tile_Index in 1 .. Tis.Tile_Count loop
         declare
            Tile : Tile_Entry (Tis.Tile_Pixel_Count);
         begin
            for I in Tile.Palette'Range loop
               Tis.Get (Tile.Palette (I).B);
               Tis.Get (Tile.Palette (I).G);
               Tis.Get (Tile.Palette (I).R);
               Tis.Get (Tile.Palette (I).A);
            end loop;
            for I in 1 .. Tile.Size loop
               Tis.Get (Tile.Pixels (I));
            end loop;
         end;
      end loop;
   end Load;

end Chaos.Resources.Tis;
