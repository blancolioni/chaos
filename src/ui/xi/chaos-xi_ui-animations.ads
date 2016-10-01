private with Ada.Containers.Vectors;

with Xi.Texture;

with Chaos.Resources.Bam;

with Chaos.Animations;

package Chaos.Xi_UI.Animations is

   type Xi_Animation_Record is
     new Chaos.Animations.Chaos_Animation_Record with private;

   type Xi_Animation is access all Xi_Animation_Record'Class;

   overriding procedure Add_Frame
     (Animation : in out Xi_Animation_Record;
      Frame         : Chaos.Resources.Bam.Frame_Colours);

   function Texture
     (Animation   : Xi_Animation_Record'Class;
      Frame_Index : Positive)
      return Xi.Texture.Xi_Texture;

private

   package Texture_Vectors is
     new Ada.Containers.Vectors (Positive, Xi.Texture.Xi_Texture,
                                 Xi.Texture."=");

   type Xi_Animation_Record is
     new Chaos.Animations.Chaos_Animation_Record with
      record
         Frame_Textures : Texture_Vectors.Vector;
      end record;

end Chaos.Xi_UI.Animations;
