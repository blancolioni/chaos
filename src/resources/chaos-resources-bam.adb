package body Chaos.Resources.Bam is

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Bam : in out Bam_Resource)
   is
   begin
      Bam.Set_Offset (8);
      Bam.Get (Bam.Frame_Entry_Count);
      Bam.Get (Bam.Cycle_Count);
      Bam.Get (Bam.Compressed_Index);
      Bam.Get (Bam.Frame_Entry_Offset);
      Bam.Get (Bam.Palette_Offset);
      Bam.Get (Bam.Frame_Lookup_Offset);
   end Load;

end Chaos.Resources.Bam;
