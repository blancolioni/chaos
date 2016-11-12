with Chaos.Areas;

package body Chaos.Actors.Visibility is

   procedure Add_Actor
     (Groups   : in out Actor_Groups;
      Actor : Chaos.Actors.Chaos_Actor;
      Attitude : Chaos.Teams.Chaos_Attitude);

   ------------------
   -- Add_Actor --
   ------------------

   procedure Add_Actor
     (Groups   : in out Actor_Groups;
      Actor    : Chaos.Actors.Chaos_Actor;
      Attitude : Chaos.Teams.Chaos_Attitude)
   is
   begin
      if not Groups.Gs (Attitude).Contains (Actor) then
         Groups.Gs (Attitude).Append (Actor);
      end if;
   end Add_Actor;

   ---------------------------
   -- Add_Actor_Can_Be_Seen --
   ---------------------------

   procedure Add_Actor_Can_Be_Seen
     (Actor  : Chaos.Actors.Chaos_Actor;
      Groups : out Actor_Groups)
   is
      procedure Process (Seen : Chaos.Actors.Chaos_Actor);

      -------------
      -- Process --
      -------------

      procedure Process (Seen : Chaos.Actors.Chaos_Actor) is
      begin
         Add_Actor (Groups, Seen,
                    Chaos.Teams.Attitude (Seen.Team, Actor.Team));
      end Process;

   begin
      Actor.Area.Scan_Visible_To_Actors (Actor, Process'Access);
   end Add_Actor_Can_Be_Seen;

   --------------------------
   -- Add_Actor_Can_See --
   --------------------------

   procedure Add_Actor_Can_See
     (Actor : Chaos.Actors.Chaos_Actor;
      Groups   : out Actor_Groups)
   is
      procedure Process (Seen : Chaos.Actors.Chaos_Actor);

      -------------
      -- Process --
      -------------

      procedure Process (Seen : Chaos.Actors.Chaos_Actor) is
      begin
         Add_Actor (Groups, Seen,
                    Chaos.Teams.Attitude (Seen.Team, Actor.Team));
      end Process;

   begin
      Actor.Area.Scan_Visible_Actors (Actor, Process'Access);
   end Add_Actor_Can_See;

   -------------
   -- Can_See --
   -------------

   function Can_See
     (Actor    : Chaos_Actor;
      Location : Chaos.Locations.Square_Location)
      return Boolean
   is
   begin
      return Actor.Area.Visible (Actor.Visible_Range (Actor.Area.Visibility),
                                 Actor.Location, Location);
   end Can_See;

   -------------
   -- Can_See --
   -------------

   function Can_See
     (Actor  : Chaos_Actor;
      Target : not null access constant
        Chaos.Objects.Root_Chaos_Object_Record'Class)
      return Boolean
   is
   begin
      if Target.all in Chaos_Actor_Record'Class then
         return Actor.Area.Visible
           (Actor.Visible_Range (Actor.Area.Visibility),
            Actor.Location,
            Chaos_Actor (Target).Location);
      elsif Target.all in Chaos.Creatures.Chaos_Creature_Record'Class then
         declare
            Creature : constant Chaos.Creatures.Chaos_Creature :=
                         Chaos.Creatures.Chaos_Creature (Target);
         begin
            if Actor.Area.Has_Actor (Creature) then
               return Actor.Area.Visible
                 (Actor.Location, Actor.Area.Actor (Creature).Location);
            else
               return False;
            end if;
         end;
      else
         return False;
      end if;
   end Can_See;

   -----------
   -- Clear --
   -----------

   procedure Clear (Groups : in out Actor_Groups) is
   begin
      for G in Groups.Gs'Range loop
         Groups.Gs (G).Clear;
      end loop;
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains
     (Groups    : Actor_Groups;
      Attitude  : Chaos.Teams.Chaos_Attitude;
      Reference : Chaos.Actors.Chaos_Actor)
      return Boolean
   is
   begin
      return Groups.Gs (Attitude).Contains (Reference);
   end Contains;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty
     (Groups   : Actor_Groups;
      Attitude : Chaos.Teams.Chaos_Attitude)
      return Boolean
   is
   begin
      return Groups.Gs (Attitude).Is_Empty;
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Groups   : Actor_Groups;
      Attitude : Chaos.Teams.Chaos_Attitude;
      Process  : not null access
        procedure (Reference : Chaos.Actors.Chaos_Actor))
   is
   begin
      for Ref of Groups.Gs (Attitude) loop
         Process (Ref);
      end loop;
   end Iterate;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Groups : Actor_Groups;
      Process : not null access
        procedure (Reference : Chaos.Actors.Chaos_Actor))
   is
   begin
      for G in Chaos.Teams.Chaos_Attitude loop
         Iterate (Groups, G, Process);
      end loop;
   end Iterate;

end Chaos.Actors.Visibility;
