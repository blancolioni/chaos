with Chaos.Infinity_Engine;

package body Chaos.Animations.Actors is

   ----------------------------
   -- Get_Standing_Animation --
   ----------------------------

   function Get_Standing_Animation
     (Actor : Chaos.Actors.Chaos_Actor)
      return Chaos_Animation
   is
      Code : constant String :=
               (if Actor.Creature.Animation_Id > 0
                then Chaos.Infinity_Engine.Animation_Code
                  (Actor.Creature.Animation_Id)
                else
                'C' & Actor.Creature.Race.Animation_Code
                & 'F' & Actor.Creature.Class.Animation_Code)
                & "1G1";
   begin
      return Get_Animation (Code, 9);
   end Get_Standing_Animation;

end Chaos.Animations.Actors;
