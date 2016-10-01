package body Chaos.Resources.Bam is

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Bam : in out Bam_Resource)
   is
      Frame_Lookup_Count : Natural := 0;
   begin
      Bam.Set_Offset (8);
      Bam.Get (Bam.Frame_Entry_Count);
      Bam.Get (Bam.Cycle_Count);
      Bam.Get (Bam.Compressed_Index);
      Bam.Get (Bam.Frame_Entry_Offset);
      Bam.Get (Bam.Palette_Offset);
      Bam.Get (Bam.Frame_Lookup_Offset);

      Bam.Set_Offset (Bam.Palette_Offset);
      for I in Bam.Palette'Range loop
         Bam.Get (Bam.Palette (I).B);
         Bam.Get (Bam.Palette (I).G);
         Bam.Get (Bam.Palette (I).R);
         Bam.Palette (I).A := (if I = 0 then 0 else 255);
         Bam.Skip (1);
      end loop;

      Bam.Set_Offset (Bam.Frame_Entry_Offset);
      for I in 1 .. Bam.Frame_Entry_Count loop
         declare
            Frame : Frame_Entry;
         begin
            Bam.Get (Frame.Width);
            Bam.Get (Frame.Height);
            Bam.Get (Frame.Center_X);
            Bam.Get (Frame.Center_Y);
            Bam.Get (Frame.Frame_Data_Offset);
            Frame.RLE_Compressed :=
              (Frame.Frame_Data_Offset and 2 ** 31) = 0;
            Frame.Frame_Data_Offset :=
              Frame.Frame_Data_Offset and 2 ** 31 - 1;
            Bam.Push_Offset (Frame.Frame_Data_Offset);

            declare
               Remaining_RLE : Natural := 0;
               Index         : Word_8;
               Size          : constant Word_32 :=
                                 Word_32 (Frame.Width)
                                 * Word_32 (Frame.Height);
               Pixels        : Frame_Pixels (1 .. Size);
            begin
               for J in Pixels'Range loop
                  if Remaining_RLE > 0 then
                     Remaining_RLE := Remaining_RLE - 1;
                     Pixels (J) := Bam.Compressed_Index;
                  else
                     Bam.Get (Index);
                     Pixels (J) := Index;

                     if Frame.RLE_Compressed
                       and then Index = Bam.Compressed_Index
                     then
                        Bam.Get (Index);
                        Remaining_RLE := Natural (Index);
                     end if;
                  end if;
               end loop;
               Frame.Frame_Data := new Frame_Pixels'(Pixels);
               Bam.Frame_Entries.Append (Frame);
            end;

            Bam.Pop_Offset;

         end;
      end loop;

      for I in 1 .. Bam.Cycle_Count loop
         declare
            Cycle : Cycle_Entry;
            Count : Word_16;
            First : Word_16;
         begin
            Bam.Get (Count);
            Bam.Get (First);
            Cycle.Frame_Count := Natural (Count);
            Cycle.First_Frame := Positive (First + 1);
            Frame_Lookup_Count :=
              Natural'Max (Frame_Lookup_Count,
                           Cycle.First_Frame + Cycle.Frame_Count - 1);
            Bam.Cycle_Entries.Append (Cycle);
         end;
      end loop;

      Bam.Set_Offset (Bam.Frame_Lookup_Offset);

      for I in 1 .. Frame_Lookup_Count loop
         declare
            X : Word_16;
         begin
            Bam.Get (X);
            Bam.Frame_Lookup.Append (Natural (X) + 1);
         end;
      end loop;

   end Load;

end Chaos.Resources.Bam;
