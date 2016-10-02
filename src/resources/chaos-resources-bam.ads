with Ada.Containers.Vectors;

package Chaos.Resources.Bam is

   use WL.Binary_IO;

   type Frame_Pixels is array (Word_32 range <>) of Word_8;
   type Frame_Pixel_Access is access Frame_Pixels;

   type Frame_Entry is
      record
         Width, Height      : Word_16;
         Center_X, Center_Y : Word_16;
         Frame_Data_Offset  : Word_32;
         RLE_Compressed     : Boolean;
         Frame_Data         : Frame_Pixel_Access;
      end record;

   package Frame_Entry_Vectors is
     new Ada.Containers.Vectors (Positive, Frame_Entry);

   Empty_Frame : constant Frame_Entry :=
                   (0, 0, 0, 0, 0, False, null);

   type Cycle_Entry is
      record
         Frame_Count : Natural;
         First_Frame : Positive;
      end record;

   package Cycle_Entry_Vectors is
     new Ada.Containers.Vectors (Positive, Cycle_Entry);

   package Frame_Index_Vectors is
     new Ada.Containers.Vectors (Positive, Positive);

   type Bam_Resource is
     new Chaos_Resource with
      record
         Frame_Entry_Count   : Word_16;
         Cycle_Count         : Word_8;
         Compressed_Index    : Word_8;
         Frame_Entry_Offset  : Word_32;
         Palette_Offset      : Word_32;
         Frame_Lookup_Offset : Word_32;
         Palette             : Resource_Palette;
         Frame_Entries       : Frame_Entry_Vectors.Vector;
         Cycle_Entries       : Cycle_Entry_Vectors.Vector;
         Frame_Lookup        : Frame_Index_Vectors.Vector;
      end record;

   overriding function Signature
     (Bam : Bam_Resource)
      return String
   is ("BAM ");

   overriding procedure Load
     (Bam : in out Bam_Resource);

end Chaos.Resources.Bam;
