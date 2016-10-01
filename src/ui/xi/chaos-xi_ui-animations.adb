with WL.Binary_IO;

with Xi.Color;

package body Chaos.Xi_UI.Animations is

   ---------------
   -- Add_Frame --
   ---------------

   overriding procedure Add_Frame
     (Animation : in out Xi_Animation_Record;
      Frame         : Chaos.Resources.Bam.Frame_Colours)
   is
      use WL.Binary_IO;
      Tex_Data : Xi.Color.Xi_Color_2D_Array
        (Frame'Range (1), Frame'Range (2));
      Index    : Word_32 := 0;
   begin

      Chaos.Animations.Chaos_Animation_Record (Animation).Add_Frame (Frame);

      for Y in Tex_Data'Range (2) loop
         for X in Tex_Data'Range (1) loop
            Index := Index + 1;
            declare
               use Xi;

               Colour : constant Chaos.Resources.Bam.Bam_Colour :=
                          Frame (X, Y);
            begin
               Tex_Data (X, Y) :=
                 (Red => Xi_Float (Colour.R) / 255.0,
                  Green => Xi_Float (Colour.G) / 255.0,
                  Blue => Xi_Float (Colour.B) / 255.0,
                  Alpha => Xi_Float (Colour.A) / 255.0);
            end;
         end loop;
      end loop;

      Animation.Frame_Textures.Append
        (Xi.Texture.Create_From_Data
           ("", Tex_Data));

   end Add_Frame;

   -------------
   -- Texture --
   -------------

   function Texture
     (Animation   : Xi_Animation_Record'Class;
      Frame_Index : Positive)
      return Xi.Texture.Xi_Texture
   is
   begin
      return Animation.Frame_Textures.Element (Frame_Index);
   end Texture;

end Chaos.Xi_UI.Animations;
