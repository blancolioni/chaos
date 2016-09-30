--  with Ada.Containers.Vectors;

--  with Chaos.Commands.Attacks;
--  with Chaos.Commands.Meta_Commands;
--  with Chaos.Commands.Moves;

package body Chaos.Party is

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

   ----------------------
   -- Get_Party_Member --
   ----------------------

   function Get_Party_Member
     (Party    : Chaos_Party_Record'Class;
      Position : Party_Member_Index)
      return Chaos.Actors.Chaos_Actor
   is
   begin
      return Party.Members (Position);
   end Get_Party_Member;

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

end Chaos.Party;
