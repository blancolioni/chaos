with Chaos.Areas;

with Chaos.Actors.Attacks;

--  with Chaos.UI.Logging;

package body Chaos.Commands.Attacks is

   type Root_Attack_Command is
     new Root_Command_Type with
      record
         Area           : Chaos.Areas.Chaos_Area;
         Attacker       : Chaos.Actors.Chaos_Actor;
         Defender       : Chaos.Actors.Chaos_Actor;
         Power          : Chaos.Powers.Chaos_Power;
         Normal_Range   : Natural;
         Maximum_Range  : Natural;
         Executing_Main : Boolean;
         --           Choices        : Chaos.Powers.List_Of_Effects.List;
         --           Current_Choice : Chaos.Powers.List_Of_Effects.Cursor;
      end record;

   overriding function Tag
     (Command : Root_Attack_Command)
      return String
   is (Command.Power.Identifier);

   overriding function Group
     (Command : Root_Attack_Command)
      return String
   is ("attack");

   overriding function Class
     (Command : Root_Attack_Command)
      return String
   is (Chaos.Powers.Power_Use_Class'Image
       (Command.Power.Use_Class));

   overriding function Available
     (Command : Root_Attack_Command)
      return Boolean
   is (Command.Attacker.Has_Standard_Action);

   overriding function Is_Attack
     (Command : Root_Attack_Command)
      return Boolean
   is (True);

   overriding function Destination_OK
     (Command : Root_Attack_Command;
      Location : Chaos.Locations.Square_Location)
      return Boolean;

   overriding function Get_Path_Feedback
     (Command   : Root_Attack_Command;
      Location  : Chaos.Locations.Square_Location)
      return Path_Feedback;

   overriding function Area_Effect
     (Command : Root_Attack_Command;
      Location : Chaos.Locations.Square_Location)
      return Chaos.Locations.Square_Path;

   overriding function Path_To
     (Command  : Root_Attack_Command;
      Location : Chaos.Locations.Square_Location)
      return Chaos.Locations.Square_Path;

   overriding procedure Execute
     (Command  : in out Root_Attack_Command;
      Location : Chaos.Locations.Square_Location);

   overriding function More
     (Command : Root_Attack_Command)
      return Boolean
   is (False);

--   is (Chaos.Powers.List_Of_Effects.Has_Element (Command.Current_Choice));

   -----------------
   -- Area_Effect --
   -----------------

--     overriding function Area_Effect
--       (Command : Root_Attack_Command;
--        Location : Chaos.Locations.Square_Location)
--        return Chaos.Locations.Square_Path
--     is
--     begin
--        return Chaos.Powers.Area_Effect
--          (Command.Power,
--           Chaos.Creatures.Location (Command.Attacker),
--           Location);
--     end Area_Effect;

   -----------------
   -- Area_Effect --
   -----------------

   overriding function Area_Effect
     (Command  : Root_Attack_Command;
      Location : Chaos.Locations.Square_Location)
      return Chaos.Locations.Square_Path
   is
      pragma Unreferenced (Command, Location);
   begin
      return Chaos.Locations.No_Path;
   end Area_Effect;

   --------------------
   -- Destination_OK --
   --------------------

   overriding function Destination_OK
     (Command : Root_Attack_Command;
      Location : Chaos.Locations.Square_Location)
      return Boolean
   is
      use Chaos.Locations;
      use Chaos.Powers;
      use type Chaos.Actors.Chaos_Actor;
      From : constant Chaos.Locations.Square_Location :=
               Command.Attacker.Location;
      Target : constant Chaos.Actors.Chaos_Actor :=
                 Command.Area.Actor (Location);
   begin

      if Command.Executing_Main then
         if Target = null
           and then Command.Power.Attack_Target = One_Creature
         then
            return False;
         end if;

         if Command.Power.Attack_Target = Blast then
            return Location /= From
              and then abs (Location.X - From.X) <= 1
              and then abs (Location.Y - From.Y) <= 1;
         end if;

         declare
            Length : constant Natural :=
                       Natural'Max
                         (abs (Location.X - From.X),
                          abs (Location.Y - From.Y));
         begin
            if Length > 1 and then Length > Command.Maximum_Range then
               return False;
            end if;
         end;

         return True;
      else
         return False;
--           return Chaos.Powers.Can_Target_Effect
--             (Command.Attacker, Command.Defender, Location,
--              Chaos.Powers.List_Of_Effects.Element
--                (Command.Current_Choice));
      end if;
   end Destination_OK;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command  : in out Root_Attack_Command;
      Location : Chaos.Locations.Square_Location)
   is
      use Chaos.Powers;
   begin
      if Command.Executing_Main then
         declare
            Target : constant Chaos.Actors.Chaos_Actor :=
                       Command.Area.Actor (Location);
         begin
            if Command.Power.Class = Attack then
               if Command.Power.Attack_Target = One_Creature then
                  Command.Defender := Target;
                  Chaos.Actors.Attacks.Power_Attack
                    (Command.Attacker, Target, Command.Power);
               else
                  Chaos.Actors.Attacks.Power_Attack
                    (Command.Attacker, Location, Command.Power);
               end if;
            else
               Command.Defender := Target;
--                 Chaos.Powers.Use_Power (Command.Attacker, Target,
--                                         Command.Power);
            end if;
         end;

         Command.Executing_Main := False;

--           if not Command.Choices.Is_Empty then
--              Command.Current_Choice := Command.Choices.First;
--           end if;
      else
         null;
--           declare
--              Effect : constant Chaos.Db.Effect_Reference :=
--                         Chaos.Powers.List_Of_Effects.Element
--                           (Command.Current_Choice);
--           begin
--              Chaos.Powers.List_Of_Effects.Next (Command.Current_Choice);
--
--              declare
--                 Target : constant Chaos.Actors.Chaos_Actor :=
--                            Chaos.Db.Creature.Get_Reference_By_Location
--                              (Integer (Location.X),
--                               Integer (Location.Y),
--                               Integer (Location.Altitude));
--              begin
--                 Chaos.Powers.Apply_Effect
--                   (Command.Attacker, Command.Defender, Target, Effect);
--              end;
--           end;
      end if;

      --        if Chaos.Powers.List_Of_Effects.Has_Element
--  (Command.Current_Choice) then
--           Chaos.UI.Logging.Current_Logger.Log
--             (Chaos.Creatures.Name (Command.Attacker),
--              Chaos.Conditions.Condition_Tag
--                (Chaos.Effects.Get_Condition
--                     (Chaos.Powers.List_Of_Effects.Element
--                          (Command.Current_Choice))));
--        end if;

   end Execute;

   -----------------------
   -- Get_Path_Feedback --
   -----------------------

   overriding function Get_Path_Feedback
     (Command : Root_Attack_Command;
      Location : Chaos.Locations.Square_Location)
      return Path_Feedback
   is
      From : constant Chaos.Locations.Square_Location :=
               Command.Attacker.Location;
      Length   : constant Natural :=
                   Natural'Max
                     (abs (Location.X - From.X),
                      abs (Location.Y - From.Y));
   begin
      if Length > Command.Normal_Range then
         return Long;
      else
         return Attack;
      end if;
   end Get_Path_Feedback;

   -------------
   -- Path_To --
   -------------

   overriding function Path_To
     (Command  : Root_Attack_Command;
      Location : Chaos.Locations.Square_Location)
      return Chaos.Locations.Square_Path
   is
   begin
      return Path : Chaos.Locations.Square_Path do
         Chaos.Locations.Append (Path, Command.Attacker.Location);
         Chaos.Locations.Append (Path, Location);
      end return;
   end Path_To;

   ------------------
   -- Power_Attack --
   ------------------

   function Power_Attack
     (Actor       : Chaos.Actors.Chaos_Actor;
      Power       : Chaos.Powers.Chaos_Power;
      Environment : not null access constant
        Chaos.Commands.Command_Environment_Interface'Class)
      return Command_Type
   is
   begin
      return new Root_Attack_Command'
        (Environment    => Environment,
         Area           => Chaos.Areas.Chaos_Area (Environment),
         Attacker       => Actor,
         Defender       => null,
         Power          => Power,
         Normal_Range   => Power.Short_Range,
         Maximum_Range  => Power.Long_Range,
         Executing_Main => True);
--
--           Choices        => Chaos.Powers.List_Of_Effects.Empty_List,
--           Current_Choice => Chaos.Powers.List_Of_Effects.No_Element);
   end Power_Attack;

end Chaos.Commands.Attacks;
