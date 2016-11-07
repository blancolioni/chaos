with Ada.Characters.Handling;

with Chaos.Logging;

package body Chaos.Resources.Key is

   -------------------
   -- Get_Biff_Path --
   -------------------

   function Get_Resource_Location
     (Key       : Key_Resource'Class;
      Reference : Resource_Reference;
      Res_Type  : Resource_Type;
      Locator   : out WL.Binary_IO.Word_32)
      return String
   is
      use Ada.Characters.Handling;
      use Ada.Strings.Unbounded;
      Ref : Resource_Reference := Reference;
   begin
      for I in Ref'Range loop
         if Ref (I) = ' ' then
            Ref (I) := Character'Val (0);
         elsif Is_Lower (Ref (I)) then
            Ref (I) := To_Upper (Ref (I));
         end if;
      end loop;

      declare
         Key_Text : constant String :=
                      String (Reference)
                    & Character'Val (Res_Type / 256)
                      & Character'Val (Res_Type mod 256);
         Position : constant Resource_Entry_Maps.Cursor :=
                      Key.Resource_Map.Find (Key_Text);
      begin
         if Resource_Entry_Maps.Has_Element (Position) then
            declare
               Res : constant Resource_Entry :=
                       Resource_Entry_Maps.Element (Position);
            begin
               Locator := Res.Locator mod 2 ** 20;
               return To_String
                 (Key.Biff_Entries.Element (Res.Biff_Index).Path);
            end;
         end if;
      end;

      Chaos.Logging.Log
        ("KEY",
         "resource " & To_String (Reference)
         & " type "
         & Hex_Image (Res_Type) & " not found");
      return "";
   end Get_Resource_Location;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Key : in out Key_Resource)
   is
      Biff_Entry_Count      : Word_32;
      Resource_Entry_Count : Word_32;
      Biff_Entry_Start      : Word_32;
      Resource_Entry_Start : Word_32;
   begin
      Key.Set_Offset (16#0008#);
      Key.Get (Biff_Entry_Count);
      Key.Get (Resource_Entry_Count);
      Key.Get (Biff_Entry_Start);
      Key.Get (Resource_Entry_Start);

      Chaos.Logging.Log
        ("KEY", "Biff entries:" & Biff_Entry_Count'Img);

      Key.Set_Offset (Biff_Entry_Start);
      for I in 1 .. Biff_Entry_Count loop
         declare
            Biff : Biff_Entry;
            File_Name_Offset : Word_32;
            File_Name_Length : Word_16;
            Location_Flags   : Word_16;
         begin
            Key.Get (Biff.Length);
            Key.Get (File_Name_Offset);
            Key.Get (File_Name_Length);
            Biff.Path :=
              Ada.Strings.Unbounded.To_Unbounded_String
                (Key.Get (File_Name_Offset, Natural (File_Name_Length)));
            Key.Get (Location_Flags);
            Biff.Data := Location_Flags mod 2 = 1;
            Location_Flags := Location_Flags / 2;
            Biff.Cache := Location_Flags mod 2 = 1;
            Location_Flags := Location_Flags / 2;
            Biff.CD := 1;
            while Location_Flags /= 0
              and then Location_Flags mod 2 = 0
            loop
               Location_Flags := Location_Flags / 2;
               Biff.CD := Biff.CD + 1;
            end loop;
            Key.Biff_Entries.Append (Biff);
         end;
      end loop;

      Key.Set_Offset (Resource_Entry_Start);
      for I in 1 .. Resource_Entry_Count loop
         declare
            Resource : Resource_Entry;
         begin
            Key.Get (Resource.Resource_Name);
            Key.Get (Resource.Resource_Type);
            Key.Get (Resource.Locator);
            Resource.Biff_Index :=
              Natural (Resource.Locator / 2 ** 20);
            Key.Resource_Entries.Append (Resource);
            declare
               Key_Text : constant String :=
                            String (Resource.Resource_Name)
                          & Character'Val (Resource.Resource_Type / 256)
                            & Character'Val (Resource.Resource_Type mod 256);
            begin
               Key.Resource_Map.Insert (Key_Text, Resource);
            end;
         end;
      end loop;

   end Load;

end Chaos.Resources.Key;
