with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Equal_Case_Insensitive;
with Ada.Strings.Fixed.Hash_Case_Insensitive;

with Ada.Directories;

with Cairo.Png;

with Chaos.Paths;

with Chaos.UI.Gtk_UI.Png;

package body Chaos.UI.Gtk_UI.Resources is

   type Image_Record is
      record
         Width, Height : Natural;
         Surface       : Cairo.Cairo_Surface;
      end record;

   package Image_Resource_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Image_Record,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   Image_Map : Image_Resource_Maps.Map;

   ------------------
   -- Image_Height --
   ------------------

   function Image_Height
     (Name : String)
     return Natural
   is
   begin
      if Image_Map.Contains (Name) then
         return Image_Map.Element (Name).Height;
      else
         return 0;
      end if;
   end Image_Height;

   --------------------
   -- Image_Resource --
   --------------------

   function Image_Surface
     (Name : String)
      return Cairo.Cairo_Surface
   is
   begin
      if not Image_Map.Contains (Name) then
         declare
            Path : constant String :=
                     Chaos.Paths.Config_File
                       ("images/" & Name & ".png");
         begin
            if Ada.Directories.Exists (Path) then
               declare
                  Image : Image_Record;
               begin
                  Image.Surface := Cairo.Png.Create_From_Png (Path);
                  Chaos.UI.Gtk_UI.Png.Get_Png_Size
                    (Path, Image.Width, Image.Height);
                  Image_Map.Insert (Name, Image);
               end;
            else
               return Cairo.Null_Surface;
            end if;
         end;
      end if;

      return Image_Map.Element (Name).Surface;
   end Image_Surface;

   -----------------
   -- Image_Width --
   -----------------

   function Image_Width
     (Name : String)
      return Natural
   is
   begin
      if Image_Map.Contains (Name) then
         return Image_Map.Element (Name).Width;
      else
         return 0;
      end if;
   end Image_Width;

end Chaos.UI.Gtk_UI.Resources;
