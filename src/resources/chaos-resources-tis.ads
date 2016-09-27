with Ada.Containers.Indefinite_Vectors;

package Chaos.Resources.Tis is

   use WL.Binary_IO;

   type Tile_Colour is
      record
         B, G, R, A : Word_8;
      end record;

   type Tile_Palette is array (Word_8) of Tile_Colour;
   type Tile_Pixels is array (Word_32 range <>) of Word_8;

   type Tile_Entry (Size : Word_32) is
      record
         Palette : Tile_Palette;
         Pixels  : Tile_Pixels (1 .. Size);
      end record;

   package Tile_Entry_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Tile_Entry);

   type Tis_Resource is
     new Chaos_Resource with
      record
         Read_Header      : Boolean;
         Tile_Count       : Word_32;
         Tile_Length      : Word_32;
         Tile_Offset      : Word_32;
         Tile_Pixel_Count : Word_32;
         Tiles            : Tile_Entry_Vectors.Vector;
      end record;

   overriding function Signature
     (Tis : Tis_Resource)
      return String
   is ("TIS ");

   overriding function Has_Header
     (Tis : Tis_Resource)
      return Boolean
   is (Tis.Read_Header);

   overriding procedure Load
     (Tis : in out Tis_Resource);

end Chaos.Resources.Tis;
