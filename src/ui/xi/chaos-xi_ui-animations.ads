private with Ada.Containers.Vectors;
private with WL.String_Maps;

with Xi.Texture;

with Chaos.Resources;

with Chaos.Animations;

package Chaos.Xi_UI.Animations is

   type Xi_Animation_Record is
     new Chaos.Animations.Chaos_Animation_Record with private;

   type Xi_Animation is access all Xi_Animation_Record'Class;

--     type Colour_Replacement is
--        record
--           Colour_Index : WL.Binary_IO.Word_8;
--           Colour       : Chaos.Resources.Bam.Bam_Colour;
--        end record;
--
--     type Colour_Replacement_Array is
--       array (Positive range <>) of Colour_Replacement;
--
--     No_Colour_Replacement : Colour_Replacement_Array (1 .. 0);

   function Texture
     (Animation   : in out Xi_Animation_Record'Class;
      Frame_Index : Positive)
      return Xi.Texture.Xi_Texture;

   function Texture
     (Animation   : in out Xi_Animation_Record'Class;
      Identifier  : String;
      Frame_Index : Positive;
      Palette     : Chaos.Resources.Resource_Palette)
      return Xi.Texture.Xi_Texture;

private

   package Texture_Vectors is
     new Ada.Containers.Vectors (Positive, Xi.Texture.Xi_Texture,
                                 Xi.Texture."=");

   package Custom_Texture_Maps is
     new WL.String_Maps (Texture_Vectors.Vector, Texture_Vectors."=");

   type Xi_Animation_Record is
     new Chaos.Animations.Chaos_Animation_Record with
      record
         Frame_Textures  : Texture_Vectors.Vector;
         Custom_Textures : Custom_Texture_Maps.Map;
      end record;

end Chaos.Xi_UI.Animations;
