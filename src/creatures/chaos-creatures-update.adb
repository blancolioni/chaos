with Chaos.UI;
with Chaos.World.Map;

with Chaos.Creatures.Attacks;
with Chaos.Creatures.Hostiles;
with Chaos.Creatures.Moving;
with Chaos.Creatures.Visibility;

with Chaos.Players;
with Chaos.Powers;

with Chaos.Db.Creature;
with Chaos.Db.Has_Power;
with Chaos.Db.Power;
with Chaos.Db.Relationship;

package body Chaos.Creatures.Update is

   procedure Create_Combat_Actions
     (Creature : Chaos.Actors.Chaos_Actor;
      Finished : out Boolean);

   procedure Engage
     (Creature : Chaos.Actors.Chaos_Actor;
      Finished : out Boolean);

   procedure Disengage
     (Creature : Chaos.Actors.Chaos_Actor;
      Finished : out Boolean);

   procedure Attack
     (Creature : Chaos.Actors.Chaos_Actor;
      Finished : out Boolean)
     with Pre => Has_Current_Target (Creature)
     and then Has_Standard_Action (Creature);

   function Choose_Target
     (Creature : Chaos.Actors.Chaos_Actor)
      return Chaos.Actors.Chaos_Actor
     with Pre => not Has_Current_Target (Creature);

   function Attack_Power
     (Attacker : Chaos.Actors.Chaos_Actor;
      Defender : Chaos.Actors.Chaos_Actor;
      Distance : Natural)
      return Chaos.Db.Power_Reference;

   ---------
   -- Act --
   ---------

   procedure Act
     (Creature : Chaos.Actors.Chaos_Actor;
      Finished : out Boolean)
   is
      use Chaos.Db;

   begin

      Finished := True;

      if Current_Hit_Points (Creature) > 0 then
         if Has_Current_Target (Creature) then
            if Current_Hit_Points (Current_Target (Creature)) <= 0 then
               Clear_Current_Target (Creature);
            end if;
         end if;

         if not Has_Current_Target (Creature) then
            declare
               New_Target : constant Creature_Reference :=
                              Choose_Target (Creature);
            begin
               if New_Target /= Null_Creature_Reference then
                  Set_Current_Target (Creature, New_Target);
               end if;
            end;
         end if;

         if Has_Current_Target (Creature) then
            Create_Combat_Actions (Creature, Finished);
         end if;

      end if;

   end Act;

   ------------
   -- Attack --
   ------------

   procedure Attack
     (Creature : Chaos.Actors.Chaos_Actor;
      Finished : out Boolean)
   is
      use Chaos.Db;
      Target : constant Creature_Reference :=
                 Current_Target (Creature);
      Target_Location : constant Chaos.Locations.Square_Location :=
                          Chaos.Creatures.Location (Target);
      Distance        : constant Natural :=
                          Chaos.World.Attack_Range
                            (From => Chaos.Creatures.Location (Creature),
                             To   => Target_Location);
      Power           : constant Chaos.Db.Power_Reference :=
                          Attack_Power
                            (Creature, Target, Distance);
      Extra_Effects   : Chaos.Powers.List_Of_Effects.List;
   begin
      if Power /= Null_Power_Reference then
         Chaos.Creatures.Attacks.Power_Attack
           (Creature, Target, Power, Extra_Effects);
         pragma Assert (Extra_Effects.Is_Empty);
--           Chaos.Powers.Attack (Creature, Target, Power);
         Finished := False;
      else
         Finished := True;
      end if;
   end Attack;

   ------------------
   -- Attack_Power --
   ------------------

   function Attack_Power
     (Attacker : Chaos.Actors.Chaos_Actor;
      Defender : Chaos.Actors.Chaos_Actor;
      Distance : Natural)
      return Chaos.Db.Power_Reference
   is
      pragma Unreferenced (Defender);
      use Chaos.Db;
   begin
      if Distance <= 1 then
         for Has_Power of
           Chaos.Db.Has_Power.Select_By_Powered
             (Chaos.Creatures.Powered (Attacker))
         loop
            declare
               Power : constant Chaos.Db.Power.Power_Type :=
                         Chaos.Db.Power.Get (Has_Power.Power);
            begin
               if Power.Attack_Type = Melee then
                  return Power.Reference;
               end if;
            end;
         end loop;

         return Chaos.Db.Null_Power_Reference;
      end if;

      for Has_Power of
        Chaos.Db.Has_Power.Select_By_Powered
          (Chaos.Creatures.Powered (Attacker))
      loop
         declare
            Power : constant Chaos.Db.Power.Power_Type :=
                      Chaos.Db.Power.Get (Has_Power.Power);
         begin
            if Power.Attack_Type /= No_Attack_Type
              and then Power.Short_Range >= Distance
            then
               return Power.Reference;
            end if;
         end;
      end loop;

      for Has_Power of
        Chaos.Db.Has_Power.Select_By_Powered
          (Chaos.Creatures.Powered (Attacker))
      loop
         declare
            Power : constant Chaos.Db.Power.Power_Type :=
                      Chaos.Db.Power.Get (Has_Power.Power);
         begin
            if Power.Attack_Type /= No_Attack_Type
              and then Power.Long_Range >= Distance
            then
               return Power.Reference;
            end if;
         end;
      end loop;

      return Chaos.Db.Null_Power_Reference;
   end Attack_Power;

   --------------------
   -- Check_Hostiles --
   --------------------

   procedure Check_Hostiles is
      use Chaos.Db;
      use Chaos.Creatures.Visibility;
      Can_See_Player : Creature_Groups;

      procedure Set_Active (Reference : Chaos.Actors.Chaos_Actor);

      ----------------
      -- Set_Active --
      ----------------

      procedure Set_Active (Reference : Chaos.Actors.Chaos_Actor) is
      begin
         Chaos.UI.Current_Model.Active_Creature (Reference);
      end Set_Active;

   begin
      for Player_Index in Chaos.Players.Party_Member_Index loop
         declare
            Player : constant Creature_Reference :=
                       Chaos.Players.Get_Party_Creature (Player_Index);
         begin
            if Player /= Null_Creature_Reference then
               Add_Creature_Can_Be_Seen
                 (Player, Can_See_Player);
            end if;
         end;
      end loop;

      if not Visibility.Is_Empty (Can_See_Player, Hostile) then
         if not Chaos.UI.Current_UI.Active_Battle then
            Chaos.UI.Current_UI.Start_Battle;
         end if;

         for Player_Index in Chaos.Players.Party_Member_Index loop
            declare
               Player : constant Creature_Reference :=
                          Chaos.Players.Get_Party_Creature (Player_Index);
            begin
               if Player /= Null_Creature_Reference then
                  Set_Active (Player);
               end if;
            end;
         end loop;

         Iterate (Can_See_Player, Set_Active'Access);

      else
         if Chaos.UI.Current_UI.Active_Battle then
            Chaos.UI.Current_UI.End_Battle;
         end if;
      end if;

   end Check_Hostiles;

   -------------------
   -- Choose_Target --
   -------------------

   function Choose_Target
     (Creature : Chaos.Actors.Chaos_Actor)
      return Chaos.Actors.Chaos_Actor
   is
      use Chaos.Db;
      Current_Closest  : Creature_Reference := Null_Creature_Reference;
      Closest_Distance : Natural := Natural'Last;
      My_Location      : constant Chaos.Locations.Square_Location :=
                           Chaos.Creatures.Location (Creature);
   begin
      for Relationship of
        Chaos.Db.Relationship.Select_By_Teams
          (Chaos.Creatures.Team (Creature), Hostile)
      loop
         for Target of
           Chaos.Db.Creature.Select_By_Team
             (Relationship.Team_2)
         loop
            if Target.Hp > 0 then
               declare
                  Distance : constant Natural :=
                               Chaos.World.Attack_Range
                                 (My_Location,
                                  Chaos.Creatures.Location (Target));
               begin
                  if Distance < Closest_Distance then
                     Closest_Distance := Distance;
                     Current_Closest := Target.Reference;
                  end if;
               end;
            end if;
         end loop;
      end loop;
      return Current_Closest;
   end Choose_Target;

   ---------------------------
   -- Create_Combat_Actions --
   ---------------------------

   procedure Create_Combat_Actions
     (Creature : Chaos.Actors.Chaos_Actor;
      Finished : out Boolean)
   is
      use Chaos.Db;
   begin
      case Chaos.Creatures.Role (Creature) is
         when Artillery =>
            Disengage (Creature, Finished);

         when Brute =>
            Engage (Creature, Finished);

         when Controller =>
            null;

         when Lurker =>
            Disengage (Creature, Finished);

         when Minion =>
            Engage (Creature, Finished);

         when Skirmisher =>
            Engage (Creature, Finished);

         when Soldier =>
            Engage (Creature, Finished);

      end case;
   end Create_Combat_Actions;

   ---------------
   -- Disengage --
   ---------------

   procedure Disengage
     (Creature : Chaos.Actors.Chaos_Actor;
      Finished : out Boolean)
   is
      use Chaos.Db;
      use Chaos.Creatures.Hostiles;
      Current : constant Chaos.Locations.Square_Location :=
                  Chaos.Creatures.Location (Creature);
      Ns : constant Chaos.Locations.Square_Path :=
             Chaos.World.Map.Neighbours (Current);
      Target : Chaos.Locations.Square_Location;
      Found   : Boolean := False;
      Have_Move     : constant Boolean := Has_Move_Action (Creature);
      Have_Standard : constant Boolean := Has_Standard_Action (Creature);
   begin
      if Adjacent_Hostile (Creature) then
         for N of Ns loop
            if Moving.Can_Move_To (Creature, N)
              and then not Adjacent_Hostile (Creature, N)
            then
               Target := N;
               Found := True;
               exit;
            end if;
         end loop;

         if Found and then (Have_Move or else Have_Standard) then
            Use_Move_Action (Creature);
            Chaos.UI.Current_Model.Creature_Walk
              (Creature, (Current, Target));
            Finished := False;
         elsif Have_Standard then
            Attack (Creature, Finished);
         else
            Finished := True;
         end if;

      else
         if Have_Standard then
            Attack (Creature, Finished);
         else
            Finished := True;
         end if;
      end if;
   end Disengage;

   ------------
   -- Engage --
   ------------

   procedure Engage
     (Creature : Chaos.Actors.Chaos_Actor;
      Finished : out Boolean)
   is
      use Chaos.Creatures, Chaos.Creatures.Hostiles;
      Have_Move     : constant Boolean :=
                        Has_Move_Action (Creature);
      Have_Standard : constant Boolean := Has_Standard_Action (Creature);
   begin
      if not Adjacent_Hostile (Creature) then
         if Have_Move or else Have_Standard then
            declare
               Target : constant Chaos.Actors.Chaos_Actor :=
                          Chaos.Creatures.Current_Target (Creature);
               Target_Location : constant Chaos.Locations.Square_Location :=
                                   Location (Target);
               Full_Path       : constant Chaos.Locations.Square_Path :=
                                   Chaos.World.Map.Find_Path
                                     (Location (Creature), Target_Location);
               Length          : constant Natural :=
                                   Natural'Min (Full_Path'Length,
                                                Movement (Creature));
               Path            : constant Chaos.Locations.Square_Path :=
                                   Full_Path
                                     (Full_Path'First
                                      .. Full_Path'First + Length - 2);
            begin
               if Have_Move then
                  Use_Move_Action (Creature);
               else
                  Use_Standard_Action (Creature);
               end if;
               Chaos.UI.Current_Model.Creature_Walk (Creature, Path);
               Finished := False;
            end;
         else
            Finished := True;
         end if;
      else
         if Have_Standard then
            Attack (Creature, Finished);
         else
            Finished := True;
         end if;
      end if;
   end Engage;

end Chaos.Creatures.Update;
