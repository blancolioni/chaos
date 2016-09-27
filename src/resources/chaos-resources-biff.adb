with Chaos.Logging;
with Chaos.Resources.Tis;

package body Chaos.Resources.Biff is

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Biff : in out Biff_Resource)
   is
      File_Entry_Count    : Word_32;
      Tileset_Entry_Count : Word_32;
      File_Entry_Offset   : Word_32;
   begin
      Biff.Set_Offset (16#0008#);
      Biff.Get (File_Entry_Count);
      Biff.Get (Tileset_Entry_Count);
      Biff.Get (File_Entry_Offset);

      Chaos.Logging.Log ("BIFF", "file entries:" & File_Entry_Count'Img);

      Biff.Set_Offset (File_Entry_Offset);

      for I in 1 .. File_Entry_Count loop
         declare
            File : File_Entry;
         begin
            Biff.Get (File.Locator);
            Biff.Get (File.Data_Start);
            Biff.Get (File.Data_Length);
            Biff.Get (File.Resource_Type);
            Biff.Skip (2);
            Biff.File_Entries.Append (File);
         end;
      end loop;

      Chaos.Logging.Log ("BIFF", "tileset entries:" & Tileset_Entry_Count'Img);

      for I in 1 .. Tileset_Entry_Count loop
         declare
            Tile : Tileset_Entry;
         begin
            Biff.Get (Tile.Locator);
            Biff.Get (Tile.Data_Start);
            Biff.Get (Tile.Tile_Count);
            Biff.Get (Tile.Tile_Size);
            Biff.Get (Tile.Resource_Type);
            pragma Assert (Tile.Resource_Type = 16#03EB#);
            Biff.Skip (2);
            Biff.Tileset_Entries.Append (Tile);
         end;
      end loop;

   end Load;

   -------------------
   -- Open_Resource --
   -------------------

   procedure Open_Resource
     (Biff     : Biff_Resource'Class;
      Resource : in out Chaos_Resource'Class;
      Locator  : WL.Binary_IO.Word_32)
   is
   begin
      if Resource not in Chaos.Resources.Tis.Tis_Resource'Class then
         for I in 1 .. Biff.File_Entries.Last_Index loop
            declare
               File : File_Entry renames Biff.File_Entries.Element (I);
            begin
               if File.Locator mod 2 ** 20 = Locator then
                  Resource.Open (Biff, File.Data_Start, File.Data_Length);
                  return;
               end if;
            end;
         end loop;
      else
         for I in 1 .. Biff.Tileset_Entries.Last_Index loop
            declare
               Tile : Tileset_Entry renames Biff.Tileset_Entries.Element (I);
            begin
               if Tile.Locator mod 2 ** 20 = Locator then
                  Chaos.Logging.Log
                    ("BIFF",
                     "tile: start" & Tile.Data_Start'Img
                     & "; size" & Tile.Tile_Size'Img
                     & "; count" & Tile.Tile_Count'Img
                     & "; total"
                     & Word_32'Image (Tile.Tile_Size * Tile.Tile_Count));

                  declare
                     use Chaos.Resources.Tis;
                     Tis : Tis_Resource'Class renames
                             Tis_Resource'Class (Resource);
                  begin
                     Tis.Read_Header := False;
                     Tis.Tile_Count := Tile.Tile_Count;
                     Tis.Tile_Length := Tile.Tile_Size;
                     Tis.Tile_Offset := 0;
                     Tis.Tile_Pixel_Count := Tile.Tile_Size - 1024;

                     Tis.Open (Biff, Tile.Data_Start,
                               Tile.Tile_Size * Tile.Tile_Count);
                  end;
                  return;
               end if;
            end;
         end loop;
      end if;

      raise Constraint_Error with "failed to locate resource in biff";

   end Open_Resource;

end Chaos.Resources.Biff;
