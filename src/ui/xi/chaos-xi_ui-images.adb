with WL.Binary_IO;

package body Chaos.Xi_UI.Images is

   function Import_Tile
     (Tile : Chaos.Resources.Tis.Tile_Entry)
      return Xi.Color.Xi_Color_2D_Array;

   -----------------
   -- Import_Tile --
   -----------------

   function Import_Tile
     (Tile : Chaos.Resources.Tis.Tile_Entry)
      return Xi.Color.Xi_Color_2D_Array
   is
      use WL.Binary_IO;
      use Xi;
      Result : Xi.Color.Xi_Color_2D_Array (1 .. 64, 1 .. 64);
      Index  : Word_32 := 0;
   begin
      for Y in Result'Range (2) loop
         for X in Result'Range (1) loop
            Index := Index + 1;
            declare
               Pixel : constant WL.Binary_IO.Word_8 :=
                         Tile.Pixels (Index);
               In_Colour : constant Chaos.Resources.Tis.Tile_Colour :=
                             Tile.Palette (Pixel);
               Out_Color : constant Xi.Color.Xi_Color :=
                             (Red    => Xi_Float (In_Colour.R) / 255.0,
                              Green  => Xi_Float (In_Colour.G) / 255.0,
                              Blue   => Xi_Float (In_Colour.B) / 255.0,
                              Alpha  => 1.0);
            begin
               if In_Colour.G = 255
                 and then In_Colour.R = 0
                 and then In_Colour.B = 0
               then
                  Result (X, Y) := (0.0, 1.0, 0.0, 0.0);
               else
                  Result (X, Y) := Out_Color;
               end if;
            end;
         end loop;
      end loop;
      return Result;
   end Import_Tile;

   --------------------
   -- Import_Tileset --
   --------------------

   overriding procedure Import_Tileset
     (Container  : in out Xi_Image_Container_Record;
      Tileset    : Chaos.Resources.Tis.Tis_Resource'Class)
   is
   begin
      for I in 1 .. Tileset.Tiles.Last_Index loop
         Container.Tile_Images.Append (Import_Tile (Tileset.Tiles (I)));
      end loop;
   end Import_Tileset;

   ----------
   -- Tile --
   ----------

   function Tile
     (Container  : Xi_Image_Container_Record'Class;
      Index      : Positive)
      return Xi.Color.Xi_Color_2D_Array
   is
      Tile_Images : Tile_Image_Vectors.Vector renames
                      Container.Tile_Images;
   begin
      if Index <= Tile_Images.Last_Index then
         return Tile_Images.Element (Index);
      else
         raise Constraint_Error with
           "invalid tile index: index was"
           & Index'Img & " but tile count is"
           & Natural'Image (Tile_Images.Last_Index);
      end if;
   end Tile;

end Chaos.Xi_UI.Images;
