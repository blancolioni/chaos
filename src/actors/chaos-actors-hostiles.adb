with Chaos.Areas;
with Chaos.Teams;
with Chaos.Vision;

with Chaos.Actors.Visibility;

package body Chaos.Actors.Hostiles is

   ----------------------
   -- Adjacent_Hostile --
   ----------------------

   function Adjacent_Hostile
     (Actor    : Chaos.Actors.Chaos_Actor;
      Location : Chaos.Locations.Square_Location)
      return Boolean
   is
      use Chaos.Actors;
      use Chaos.Locations;
      Ns : constant Chaos.Locations.Square_Path :=
             Actor.Area.Neighbours (Location);
   begin
      for I in 1 .. Length (Ns) loop
         declare
            N : constant Square_Location := Square (Ns, I);
            Hostile : constant Chaos.Actors.Chaos_Actor :=
                        Actor.Area.Actor (N);
         begin
            if Hostile /= null then
               declare
                  use type Chaos.Teams.Chaos_Attitude;
                  Attitude : constant Chaos.Teams.Chaos_Attitude :=
                               Chaos.Teams.Attitude
                                 (Hostile.Team, Actor.Team);

               begin
                  if Attitude = Chaos.Teams.Hostile then
                     return True;
                  end if;
               end;
            end if;
         end;
      end loop;
      return False;
   end Adjacent_Hostile;

   ---------------------
   -- Visible_Hostile --
   ---------------------

   function Visible_Hostile
     (Actor : Chaos.Actors.Chaos_Actor)
      return Boolean
   is
      use Chaos.Locations;
      Loc : constant Square_Location := Actor.Location;
      Max : constant Natural :=
              Actor.Creature.Visible_Range (Chaos.Vision.Normal);
   begin
      for Y in Loc.Y - Max .. Loc.Y + Max loop
         for X in Loc.X - Max .. Loc.X + Max loop
            if (X /= Loc.X or else Y /= Loc.Y)
              and then Actor.Area.Contains_Point (X, Y)
            then
               declare
                  Hostile : constant Chaos_Actor :=
                              Actor.Area.Actor ((X, Y));
               begin
                  if Hostile /= null then
                     declare
                        use type Chaos.Teams.Chaos_Attitude;
                        Attitude : constant Chaos.Teams.Chaos_Attitude :=
                                     Chaos.Teams.Attitude
                                       (Hostile.Team, Actor.Team);
                     begin
                        if Attitude = Chaos.Teams.Hostile then
                           return True;
                        end if;
                     end;
                  end if;
               end;
            end if;
         end loop;
      end loop;
      return False;
   end Visible_Hostile;

   ----------------------------
   -- Visible_Hostile_Player --
   ----------------------------

   function Visible_Hostile_Player
     (Actor : Chaos.Actors.Chaos_Actor)
      return Boolean
   is
      V : Chaos.Actors.Visibility.Actor_Groups;
   begin
      Chaos.Actors.Visibility.Add_Actor_Can_See (Actor, V);
      return not Chaos.Actors.Visibility.Is_Empty (V, Chaos.Teams.Party);
   end Visible_Hostile_Player;

end Chaos.Actors.Hostiles;
