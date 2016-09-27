with Ada.Sequential_IO;
with Ada.Text_IO;

with System.Storage_Elements;

package body Chaos.UI.Gtk_UI.Png is

   package Storage_Element_IO is
     new Ada.Sequential_IO (System.Storage_Elements.Storage_Element);

   ------------------
   -- Get_Png_Size --
   ------------------

   procedure Get_Png_Size
     (Path          : String;
      Width, Height : out Natural)
   is
      use System.Storage_Elements;
      use Storage_Element_IO;
      Header : System.Storage_Elements.Storage_Array (1 .. 24);
      File : File_Type;
   begin
      Open (File, In_File, Path);
      for I in Header'Range loop
         Read (File, Header (I));
      end loop;
      Close (File);

      if Header (1 .. 8) /= (137, 80, 78, 71, 13, 10, 26, 10) then
         raise Constraint_Error with "not a valid png file: " & Path;
      end if;

      Width := 0;
      for I in Storage_Offset range 17 .. 20 loop
         Width := Width * 256 + Natural (Header (I));
      end loop;

      Height := 0;
      for I in Storage_Offset range 21 .. 24 loop
         Height := Height * 256 + Natural (Header (I));
      end loop;

      Ada.Text_IO.Put_Line
        (Path & ":" & Width'Img & " x" & Height'Img);

   exception
      when Name_Error =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Path & ": file not found");
         Width := 0;
         Height := 0;
      when End_Error =>
         raise Constraint_Error with
           "not a valid png file: " & Path;

   end Get_Png_Size;

end Chaos.UI.Gtk_UI.Png;
