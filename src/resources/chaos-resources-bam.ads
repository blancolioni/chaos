with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;

package Chaos.Resources.Bam is

   use WL.Binary_IO;

   type Bam_Colour is
      record
         B, G, R, A : Word_8;
      end record;

   type Bam_Palette is array (Word_8) of Bam_Colour;

   type Frame_Pixels is array (Word_32 range <>) of Word_8;

   package Frame_Pixel_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Frame_Pixels);

   type Frame_Entry is
      record
         Width, Height      : Word_16;
         Center_X, Center_Y : Word_16;
         Frame_Data_Offset  : Word_32;
         RLE_Compressed     : Boolean;
         Frame_Data         : Frame_Pixel_Vectors.Vector;
      end record;

   type Cycle_Entry is
      record
         Frame_Count : Natural;
         First_Frame : Positive;
      end record;

   package Cycle_Entry_Vectors is
     new Ada.Containers.Vectors (Positive, Cycle_Entry);

   package Frame_Index_Vectors is
     new Ada.Containers.Vectors (Positive, Word_16);

   type Bam_Resource is
     new Chaos_Resource with
      record
         Frame_Entry_Count   : Word_16;
         Cycle_Count         : Word_8;
         Compressed_Index    : Word_8;
         Frame_Entry_Offset  : Word_32;
         Palette_Offset      : Word_32;
         Frame_Lookup_Offset : Word_32;
         Palette             : Bam_Palette;
         Cycle_Entries       : Cycle_Entry_Vectors.Vector;
      end record;

   overriding function Signature
     (Bam : Bam_Resource)
      return String
   is ("BAM ");

   overriding procedure Load
     (Bam : in out Bam_Resource);

end Chaos.Resources.Bam;
