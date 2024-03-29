--  with Ada.Containers.Vectors;

--  with Chaos.Commands.Attacks;
--  with Chaos.Commands.Meta_Commands;
--  with Chaos.Commands.Moves;

with Chaos.Creatures;

package body Chaos.Party is

   ----------------------
   -- Add_Party_Member --
   ----------------------

   procedure Add_Party_Member
     (Party    : in out Chaos_Party_Record'Class;
      Actor    : Chaos.Actors.Chaos_Actor)
   is
      use type Chaos.Actors.Chaos_Actor;
   begin
      for I in Party.Members'Range loop
         if Party.Members (I) = null then
            Party.Members (I) := Actor;
            return;
         end if;
      end loop;
      raise Constraint_Error with "Add_Party_Member: party is full";
   end Add_Party_Member;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Party : in out Chaos_Party_Record'Class)
   is
   begin
      for I in Party.Members'Range loop
         Party.Members (I) := null;
      end loop;
   end Clear;

   --------------
   -- Commands --
   --------------

--     function Commands
--       (Player : Root_Player_Type'Class)
--        return Player_Commands
--     is
--        V : Command_Vectors.Vector;
--     begin
--        V.Append (Meta_Commands.End_Turn_Command (Player));
--        V.Append (Chaos.Players.Moves.Move_Command (Player));
--        V.Append (Chaos.Players.Meta_Commands.Wait_Command (Player));
--
--        for I in 1 .. Player.Creature.Power_Count loop
--           V.Append
--             (Chaos.Players.Attacks.Power_Attack
--                (Player, Player.Creature.Get_Power (I)));
--        end loop;
--
--        declare
--           Result : Player_Commands (1 .. V.Last_Index);
--        begin
--           for I in Result'Range loop
--              Result (I) := V (I);
--           end loop;
--           return Result;
--        end;
--
--     end Commands;

   ------------------
   -- Create_Party --
   ------------------

   function Create_Party return Party_Type is
   begin
      return new Chaos_Party_Record;
   end Create_Party;

   ---------------------
   -- Give_Experience --
   ---------------------

   procedure Give_Experience
     (Party : Chaos_Party_Record'Class;
      XP    : Natural)
   is
      use type Chaos.Actors.Chaos_Actor;
      Count : Natural := 0;

      Award : Natural;

      procedure Add_XP
        (Creature : in out Chaos.Creatures.Chaos_Creature_Record'Class);

      ------------
      -- Add_XP --
      ------------

      procedure Add_XP
        (Creature : in out Chaos.Creatures.Chaos_Creature_Record'Class)
      is
      begin
         Creature.Change_Experience_Points (Award);
      end Add_XP;

   begin
      for Member of Party.Members loop
         if Member /= null then
            Count := Count + 1;
         end if;
      end loop;

      Award := XP / Count + XP mod Count;

      for Member of Party.Members loop
         if Member /= null then
            Member.Creature.Update (Add_XP'Access);
            Award := XP / Count;
         end if;
      end loop;
   end Give_Experience;

   ---------------------
   -- Is_Party_Member --
   ---------------------

   function Is_Party_Member
     (Party    : Chaos_Party_Record'Class;
      Actor    : Chaos.Actors.Chaos_Actor)
      return Boolean
   is
      use type Chaos.Actors.Chaos_Actor;
   begin
      for I in Party.Members'Range loop
         if Party.Members (I) = Actor then
            return True;
         end if;
      end loop;
      return False;
   end Is_Party_Member;

   ------------------
   -- Party_Member --
   ------------------

   function Party_Member
     (Party    : Chaos_Party_Record'Class;
      Position : Party_Member_Index)
      return Chaos.Actors.Chaos_Actor
   is
   begin
      return Party.Members (Position);
   end Party_Member;

   ----------------
   -- Party_Size --
   ----------------

   function Party_Size
     (Party : Chaos_Party_Record'Class)
      return Natural
   is
      use type Chaos.Actors.Chaos_Actor;
   begin
      return Size : Natural := 0 do
         for Member of Party.Members loop
            if Member /= null then
               Size := Size + 1;
            end if;
         end loop;
      end return;
   end Party_Size;

   -------------------------
   -- Remove_Party_Member --
   -------------------------

   procedure Remove_Party_Member
     (Party    : in out Chaos_Party_Record'Class;
      Actor    : Chaos.Actors.Chaos_Actor)
   is
      use type Chaos.Actors.Chaos_Actor;
   begin
      for I in Party.Members'Range loop
         if Party.Members (I) = Actor then
            Party.Members (I) := null;
            return;
         end if;
      end loop;
      raise Constraint_Error with
        "Remove_Actor: actor not found: "
        & Actor.Identifier;
   end Remove_Party_Member;

   ---------------
   -- Take_Item --
   ---------------

   function Take_Item
     (Party  : Chaos_Party_Record'Class;
      Entity : Chaos.Entities.Chaos_Entity)
      return Chaos.Items.Chaos_Item
   is
      use type Chaos.Actors.Chaos_Actor;
      Item : Chaos.Items.Chaos_Item;

      procedure Remove_Item
        (Creature : in out Chaos.Creatures.Chaos_Creature_Record'Class);

      -----------------
      -- Remove_Item --
      -----------------

      procedure Remove_Item
        (Creature : in out Chaos.Creatures.Chaos_Creature_Record'Class)
      is
      begin
         Creature.Remove_Item (Item);
      end Remove_Item;

   begin
      for PC of Party.Members loop
         if PC /= null and then
           PC.Creature.Has_Entity (Entity)
         then
            Item := PC.Creature.Item (Entity);
            PC.Creature.Update (Remove_Item'Access);
            return Item;
         end if;
      end loop;
      raise Constraint_Error with
        "item '" & Entity.Identifier & "' not found in party";
   end Take_Item;

end Chaos.Party;
