with Chaos.Logging;
with Chaos.Infinity_Engine;

package body Chaos.Animations.Actors is

   ----------------------------
   -- Get_Standing_Animation --
   ----------------------------

   function Get_Standing_Animation
     (Actor : Chaos.Actors.Chaos_Actor)
      return Chaos_Animation
   is
      use Chaos.Actors;
      Orientation : constant Actor_Orientation := Actor.Orientation;
      Western     : constant Boolean := Orientation in South .. North;
      Offset      : constant Natural := Actor_Orientation'Pos (Orientation);
      Code        : constant String :=
                      (if Actor.Creature.Animation_Id > 0
                       then Chaos.Infinity_Engine.Animation_Code
                         (Actor.Creature.Animation_Id)
                       else
                       'C' & Actor.Creature.Race.Animation_Code
                       & 'F' & Actor.Creature.Class.Animation_Code)
                      & "1G1"
                      & (if Western then "" else "E");
      Index       : constant Positive := 9 + Offset;
   begin
      Chaos.Logging.Log
        ("ANIMATION",
         Actor.Creature.Identifier
         & ": getting " & Code & Index'Img);
      return Get_Animation (Code, Index);
   end Get_Standing_Animation;

end Chaos.Animations.Actors;
