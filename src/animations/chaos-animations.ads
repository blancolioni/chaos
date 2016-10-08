private with Ada.Containers.Vectors;

with Chaos.Resources.Bam;

package Chaos.Animations is

   type Chaos_Animation_Record is tagged private;

   type Chaos_Animation is access all Chaos_Animation_Record'Class;

   function Palette
     (Animation : Chaos_Animation_Record'Class)
      return Chaos.Resources.Resource_Palette;

   function Frame_Count (Animation : Chaos_Animation_Record) return Natural;
   function Frame
     (Animation : Chaos_Animation_Record;
      Index     : Positive)
      return Chaos.Resources.Bam.Frame_Pixel_Access;

   function Frame_Height
     (Animation : Chaos_Animation_Record'Class;
      Index     : Positive)
      return Natural;

   function Frame_Width
     (Animation : Chaos_Animation_Record'Class;
      Index     : Positive)
      return Natural;

   function Frame_Center_X
     (Animation : Chaos_Animation_Record'Class;
      Index     : Positive)
      return Natural;

   function Frame_Center_Y
     (Animation : Chaos_Animation_Record'Class;
      Index     : Positive)
      return Natural;

   procedure Add_Frame
     (Animation          : in out Chaos_Animation_Record;
      Width, Height      : Natural;
      Center_X, Center_Y : Natural;
      Frame              : Chaos.Resources.Bam.Frame_Pixel_Access);

   type Chaos_Animation_Factory is interface;

   function Create_Animation
     (Factory : Chaos_Animation_Factory)
      return Chaos.Animations.Chaos_Animation
      is abstract;

   procedure Set_Animation_Factory
     (Factory : not null access Chaos_Animation_Factory'Class);

   function Get_Animation
     (Code  : String;
      Index : Positive)
      return Chaos_Animation;

private

   use Chaos.Resources.Bam;

   type Frame_Record is
      record
         Width, Height      : Natural;
         Centre_X, Centre_Y : Natural;
         Pixels             : Frame_Pixel_Access;
      end record;

   package Frame_Vectors is
     new Ada.Containers.Vectors
       (Positive, Frame_Record);

   type Chaos_Animation_Record is tagged
      record
         Frames  : Frame_Vectors.Vector;
         Palette : Chaos.Resources.Resource_Palette;
      end record;

end Chaos.Animations;
