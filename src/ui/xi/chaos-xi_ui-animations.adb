with WL.Binary_IO;

with Chaos.Resources.Bam;

with Xi.Color;

package body Chaos.Xi_UI.Animations is

   function Create_Texture_With_Palette
     (Width, Height : Natural;
      Pixels        : Chaos.Resources.Bam.Frame_Pixels;
      Palette       : Chaos.Resources.Resource_Palette)
      return Xi.Texture.Xi_Texture;

   --------------------
   -- Create_Texture --
   --------------------

   function Create_Texture_With_Palette
     (Width, Height : Natural;
      Pixels        : Chaos.Resources.Bam.Frame_Pixels;
      Palette       : Chaos.Resources.Resource_Palette)
      return Xi.Texture.Xi_Texture
   is
      use WL.Binary_IO;
      Tex_Data : Xi.Color.Xi_Color_2D_Array (1 .. Width, 1 .. Height);
      Index    : Word_32 := 0;
   begin
      for Y in Tex_Data'Range (2) loop
         for X in Tex_Data'Range (1) loop
            Index := Index + 1;
            declare
               use Xi;
               Pixel  : constant Word_8 := Pixels (Index);
               Colour : constant Chaos.Resources.Resource_Color :=
                          Palette (Pixel);
            begin
               Tex_Data (X, Y) :=
                 (Red   => Xi_Float (Colour.R) / 255.0,
                  Green => Xi_Float (Colour.G) / 255.0,
                  Blue  => Xi_Float (Colour.B) / 255.0,
                  Alpha => Xi_Float (Colour.A) / 255.0);
            end;
         end loop;
      end loop;

      return Xi.Texture.Create_From_Data
        ("", Tex_Data);
   end Create_Texture_With_Palette;

   -------------
   -- Texture --
   -------------

   function Texture
     (Animation   : in out Xi_Animation_Record'Class;
      Identifier  : String;
      Frame_Index : Positive;
      Palette     : Chaos.Resources.Resource_Palette)
      return Xi.Texture.Xi_Texture
   is
      use WL.Binary_IO;
      Frames   : Texture_Vectors.Vector;
   begin
      if not Animation.Custom_Textures.Contains (Identifier) then
         for I in 1 .. Animation.Frame_Count loop
            Frames.Append
              (Create_Texture_With_Palette
                 (Width   => Animation.Frame_Width (I),
                  Height  => Animation.Frame_Height (I),
                  Pixels  => Animation.Frame (I).all,
                  Palette => Palette));
         end loop;
         Animation.Custom_Textures.Insert (Identifier, Frames);
      else
         Frames := Animation.Custom_Textures (Identifier);
      end if;

      return Frames (Frame_Index);
   end Texture;

   --    Chaos.Animations.Chaos_Animation_Record (Animation).Add_Frame (Frame);
   --
   --
   --     end Add_Frame;

   -------------
   -- Texture --
   -------------

   function Texture
     (Animation   : in out Xi_Animation_Record'Class;
      Frame_Index : Positive)
      return Xi.Texture.Xi_Texture
   is
   begin
      if Animation.Frame_Textures.Is_Empty then
         for I in 1 .. Animation.Frame_Count loop
            Animation.Frame_Textures.Append
              (Create_Texture_With_Palette
                 (Width  => Animation.Frame_Width (I),
                  Height => Animation.Frame_Height (I),
                  Pixels => Animation.Frame (I).all,
                  Palette => Animation.Palette));
         end loop;
      end if;

      return Animation.Frame_Textures.Element (Frame_Index);
   end Texture;

end Chaos.Xi_UI.Animations;
