with Chaos.Localisation;

package body Chaos.Resources.Tlk is

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Resource : in out Tlk_Resource)
   is
      Entry_Count    : Word_32;
      Entries_Offset : Word_32;
   begin
      Resource.Get (16#000A#, Entry_Count);
      Resource.Get (16#000E#, Entries_Offset);

      Resource.Set_Offset (18);

      for I in 1 .. Entry_Count loop
         declare
            Current     : String_Entry;
            This_Offset : Word_32;
            This_Length : Word_32;
         begin
            Resource.Get (Current.Flags);
            Resource.Get (Current.Sound);
            Resource.Get (Current.Volume);
            Resource.Get (Current.Pitch);
            Resource.Get (This_Offset);
            Resource.Get (This_Length);
            declare
               Text : constant String :=
                        Resource.Get (Entries_Offset + This_Offset,
                                      Natural (This_Length));
            begin
               Current.Text :=
                 Ada.Strings.Unbounded.To_Unbounded_String (Text);
               Chaos.Localisation.Set_Text
                 (Chaos.Localisation.Local_Text_Index (I - 1), Text);
            end;
            Resource.String_Entries.Append (Current);
         end;
      end loop;

   end Load;

end Chaos.Resources.Tlk;
