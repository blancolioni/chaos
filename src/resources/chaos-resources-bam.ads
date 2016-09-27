private with Ada.Containers.Vectors;

package Chaos.Resources.BAM is

   type Bam_Resource is
     new Chaos_Resource with private;

private

   use WL.Binary_IO;

   type File_Entry is
      record
         Resource_Locator : Word_32;
         Data_Offset      : Word_32;
         File_Size        : Word_32;
         Resource_Type    : Word_16;
         Unknown          : Word_16;
      end record;

   package File_Entry_Vectors is
     new Ada.Containers.Vectors (Positive, File_Entry);

   type Tile_Entry is
      record
         Resource_Locator : Word_32;
         Data_Offset      : Word_32;
         Tile_Count       : Word_32;
         Tile_Size        : Word_32;
         Resource_Type    : Word_16;
         Unknown          : Word_16;
      end record;

   package Tile_Entry_Vectors is
     new Ada.Containers.Vectors (Positive, Tile_Entry);

   type Bam_Resource is
     new Chaos_Resource with
      record
         File_Entries : File_Entry_Vectors.Vector;
         Tile_Entries : Tile_Entry_Vectors.Vector;
      end record;

end Chaos.Resources.BAM;
