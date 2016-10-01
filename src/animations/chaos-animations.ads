private with Ada.Containers.Indefinite_Vectors;

with Chaos.Resources.Bam;

package Chaos.Animations is

   type Chaos_Animation_Record is tagged private;

   type Chaos_Animation is access all Chaos_Animation_Record'Class;

   function Frame_Count (Animation : Chaos_Animation_Record) return Natural;
   function Frame
     (Animation : Chaos_Animation_Record;
      Index     : Positive)
      return Chaos.Resources.Bam.Frame_Colours;

   function Frame_Height
     (Animation : Chaos_Animation_Record'Class;
      Index     : Positive)
      return Natural;

   function Frame_Width
     (Animation : Chaos_Animation_Record'Class;
      Index     : Positive)
      return Natural;

   procedure Add_Frame
     (Animation     : in out Chaos_Animation_Record;
      Frame         : Chaos.Resources.Bam.Frame_Colours);

   type Chaos_Animation_Factory is interface;

   function Create_Animation
     (Factory : Chaos_Animation_Factory)
      return Chaos.Animations.Chaos_Animation
      is abstract;

   procedure Set_Animation_Factory
     (Factory : not null access Chaos_Animation_Factory'Class);

private

   use Chaos.Resources.Bam;

   package Frame_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Positive, Frame_Colours);

   type Chaos_Animation_Record is tagged
      record
         Frames : Frame_Vectors.Vector;
      end record;

   function Get_Animation
     (Code  : String;
      Index : Positive)
      return Chaos_Animation;

end Chaos.Animations;
