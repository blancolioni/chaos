with Chaos.Infinity_Engine;
with Chaos.Locations;

package body Chaos.Animations.Actors is

   function Base_Code
     (Actor : Chaos.Actors.Chaos_Actor)
      return String;

   function Get_Animation
     (Actor  : Chaos.Actors.Chaos_Actor;
      Suffix : String;
      Offset : Positive)
      return Chaos_Animation;

   ---------------
   -- Base_Code --
   ---------------

   function Base_Code
     (Actor : Chaos.Actors.Chaos_Actor)
      return String
   is
   begin
      if Actor.Creature.Animation_Id > 0 then
         return Chaos.Infinity_Engine.Animation_Code
           (Actor.Creature.Animation_Id);
      else
         return 'C' & Actor.Creature.Race.Animation_Code
           & 'F' & Actor.Creature.Class.Animation_Code
           & '1';
      end if;
   end Base_Code;

   -------------------
   -- Get_Animation --
   -------------------

   function Get_Animation
     (Actor  : Chaos.Actors.Chaos_Actor;
      Suffix : String;
      Offset : Positive)
      return Chaos_Animation
   is
      use Chaos.Locations;
      Orientation : constant Chaos.Locations.Orientation := Actor.Orientation;
      Western     : constant Boolean := Actor.Orientation in South .. North;
      Base        : constant String := Base_Code (Actor);
      Code        : constant String :=
                      Base_Code (Actor)
                    & (if Base'Length < 6
                       then Suffix & (if Western then "" else "E")
                       else "");
      Orient_Pos  : constant Natural :=
                      Chaos.Locations.Orientation'Pos (Orientation);
      Index       : constant Positive :=
                      (if Base'Length < 6
                       then Offset + Orient_Pos
                       else 1 + Orient_Pos);
   begin
      return Get_Animation (Code, Index);
   end Get_Animation;

   -------------------
   -- Get_Animation --
   -------------------

   function Get_Animation
     (Actor : Chaos.Actors.Chaos_Actor)
      return Chaos_Animation
   is
   begin
      if Actor.Has_Path then
         return Get_Animation (Actor, "G1", 1);
      else
         return Get_Animation (Actor, "G1", 25);
      end if;
   end Get_Animation;

end Chaos.Animations.Actors;
