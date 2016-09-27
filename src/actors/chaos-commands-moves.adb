with Chaos.Areas;
with Chaos.Actors.Hostiles;

with Chaos.UI;

package body Chaos.Commands.Moves is

   type Root_Move_Command is
     new Root_Command_Type with
      record
         Area     : Chaos.Areas.Chaos_Area;
         Mover    : Chaos.Actors.Chaos_Actor;
         Movement : Natural;
         Maximum  : Natural;
         Target   : Chaos.Actors.Chaos_Actor;
      end record;

   overriding function Tag
     (Command : Root_Move_Command)
      return String
   is ("move");

   overriding function Group
     (Command : Root_Move_Command)
      return String
   is ("movement");

   overriding function Class
     (Command : Root_Move_Command)
      return String
   is ("");

   overriding function Available
     (Command : Root_Move_Command)
      return Boolean
   is (Command.Mover.Has_Move_Action);

   overriding function Is_Move
     (Command : Root_Move_Command)
      return Boolean
   is (True);

   overriding function Destination_OK
     (Command : Root_Move_Command;
      Location : Chaos.Locations.Square_Location)
      return Boolean;

   overriding function Get_Path_Feedback
     (Command : Root_Move_Command;
      Location : Chaos.Locations.Square_Location)
      return Path_Feedback;

   overriding function Path_To
     (Command  : Root_Move_Command;
      Location : Chaos.Locations.Square_Location)
      return Chaos.Locations.Square_Path;

   overriding procedure Execute
     (Command : in out Root_Move_Command;
      Location : Chaos.Locations.Square_Location);

   overriding procedure On_Finish
     (Command : in out Root_Move_Command);

   --------------------
   -- Destination_OK --
   --------------------

   overriding function Destination_OK
     (Command : Root_Move_Command;
      Location : Chaos.Locations.Square_Location)
      return Boolean
   is
      use type Chaos.Actors.Chaos_Actor;
      Area   : constant Chaos.Areas.Chaos_Area :=
                 Chaos.Areas.Chaos_Area (Command.Environment);
      Start : constant Chaos.Locations.Square_Location :=
                Command.Mover.Location;
      Blocking_Actor : constant Chaos.Actors.Chaos_Actor :=
                         Area.Actor (Location);
      Blocking_Feature : constant Boolean :=
                           not Area.Passable (Location);
      Battle           : constant Boolean :=
                           Area.Part_Of_Battle (Command.Mover);
      Path : constant Chaos.Locations.Square_Path :=
               Area.Find_Path (Start, Location);
   begin

      if Blocking_Actor /= null then
         return False;
      end if;

      if Blocking_Feature then
         return False;
      end if;

      if Chaos.Locations.Length (Path) = 0 then
         return False;
      end if;

      if Battle then
         if Chaos.Locations.Length (Path) > Command.Maximum then
            return False;
         end if;
      end if;

      return True;
   end Destination_OK;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command  : in out Root_Move_Command;
      Location : Chaos.Locations.Square_Location)
   is
      use type Chaos.Actors.Chaos_Actor;
      Area   : constant Chaos.Areas.Chaos_Area :=
                 Chaos.Areas.Chaos_Area (Command.Environment);
      Target : constant Chaos.Actors.Chaos_Actor :=
                 Area.Actor (Location);
      Path     : constant Chaos.Locations.Square_Path :=
                   Command.Path_To (Location);
   begin
      if Target /= null then
         Chaos.UI.Current_Model.Creature_Walk
           (Command.Mover, Chaos.Locations.Drop_Last (Path));
         Command.Target := Target;
      else
         Chaos.UI.Current_Model.Creature_Walk
           (Command.Mover, Path);
         Command.Target := null;
      end if;
   end Execute;

   -----------------------
   -- Get_Path_Feedback --
   -----------------------

   overriding function Get_Path_Feedback
     (Command : Root_Move_Command;
      Location : Chaos.Locations.Square_Location)
      return Path_Feedback
   is
      use Chaos.Locations;
      Area   : constant Chaos.Areas.Chaos_Area :=
                 Chaos.Areas.Chaos_Area (Command.Environment);
      Path   : constant Square_Path :=
               Root_Move_Command'Class (Command).Path_To (Location);
      Danger   : constant Boolean :=
                   Chaos.Actors.Hostiles.Adjacent_Hostile (Command.Mover);
   begin
      if not Area.Part_Of_Battle (Command.Mover) then
         return OK;
      elsif Length (Path) <= Command.Mover.Maximum_Shift then
         if Danger then
            return Safe;
         else
            return OK;
         end if;
      elsif Danger then
         return Dangerous;
      elsif Length (Path) > Command.Mover.Speed then
         return Long;
      else
         return OK;
      end if;
   end Get_Path_Feedback;

   ------------------
   -- Move_Command --
   ------------------

   function Move_Command
     (Actor  : Chaos.Actors.Chaos_Actor;
      Environment : not null access constant
        Chaos.Commands.Command_Environment_Interface'Class)
      return Command_Type
   is
      Movement : constant Natural := Actor.Speed;
   begin

      return new Root_Move_Command'
        (Environment => Environment,
         Area        => Chaos.Areas.Chaos_Area (Environment),
         Mover       => Actor,
         Movement    => Actor.Speed,
         Maximum     => (if Actor.Has_Standard_Action
                         then Movement * 2
                         else Movement),
         Target      => null);
   end Move_Command;

   ---------------
   -- On_Finish --
   ---------------

   overriding procedure On_Finish
     (Command : in out Root_Move_Command)
   is
      use type Chaos.Actors.Chaos_Actor;
   begin
      if Command.Target /= null then
         Chaos.UI.Current_Model.Start_Dialog
           (Command.Mover, Command.Target);
         Command.Target := null;
      end if;
   end On_Finish;

   -------------
   -- Path_To --
   -------------

   overriding function Path_To
     (Command  : Root_Move_Command;
      Location : Chaos.Locations.Square_Location)
      return Chaos.Locations.Square_Path
   is
      use type Chaos.Locations.Square_Path;
      Start : constant Chaos.Locations.Square_Location :=
                Command.Mover.Location;
   begin
      return Start & Command.Area.Find_Path (Start, Location);
   end Path_To;

end Chaos.Commands.Moves;
