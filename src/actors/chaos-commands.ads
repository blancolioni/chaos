private with Ada.Containers.Vectors;

with Chaos.Locations;

package Chaos.Commands is

   type Command_Environment_Interface is limited interface;

   function Current_Battle
     (Environment : Command_Environment_Interface)
      return Boolean
      is abstract;

   function Find_Path
     (Environment : Command_Environment_Interface;
      Start       : Chaos.Locations.Square_Location;
      Finish      : Chaos.Locations.Square_Location)
      return Chaos.Locations.Square_Path
      is abstract;

   type Root_Command_Type is abstract tagged private;

   function Tag (Command : Root_Command_Type) return String is abstract;
   function Group (Command : Root_Command_Type) return String is abstract;
   function Class
     (Command : Root_Command_Type)
      return String is abstract;

   function Has_Destination
     (Command : Root_Command_Type)
      return Boolean
   is (True);

   function Is_Attack
     (Command : Root_Command_Type)
      return Boolean
   is (False);

   function Is_Move
     (Command : Root_Command_Type)
      return Boolean
   is (False);

   function Destination_OK
     (Command  : Root_Command_Type;
      Location : Chaos.Locations.Square_Location)
      return Boolean
   is (False);

   procedure On_Finish
     (Command : in out Root_Command_Type)
   is null;

   type Path_Feedback is (OK, Safe, Dangerous, Long, Attack);

   function Get_Path_Feedback
     (Command   : Root_Command_Type;
      Location  : Chaos.Locations.Square_Location)
      return Path_Feedback
   is (OK);

   function Area_Effect
     (Command  : Root_Command_Type;
      Location : Chaos.Locations.Square_Location)
      return Chaos.Locations.Square_Path
   is (Chaos.Locations.No_Path);

   function Path_To
     (Command  : Root_Command_Type;
      Location : Chaos.Locations.Square_Location)
      return Chaos.Locations.Square_Path
   is (Chaos.Locations.No_Path)
   with Pre'Class => Destination_OK (Command, Location);

   procedure Execute
     (Command  : in out Root_Command_Type;
      Location : Chaos.Locations.Square_Location)
   is null
     with Pre'Class => Command.Destination_OK (Location);

   procedure Execute
     (Command  : in out Root_Command_Type)
   is null
     with Pre'Class => not Command.Has_Destination;

   function More
     (Command : Root_Command_Type)
      return Boolean
   is (False);

   function Available
     (Command : Root_Command_Type)
      return Boolean
      is abstract;

   type Command_Type is access all Root_Command_Type'Class;

   type Command_Collection is private;

   function Count (Collection : Command_Collection) return Natural;
   function Command
     (Collection : Command_Collection;
      Index      : Positive)
      return Command_Type;

   procedure Append
     (Collection : in out Command_Collection;
      Command    : Command_Type);

private

   type Command_Environment is
     access constant Command_Environment_Interface'Class;

   type Root_Command_Type is abstract tagged
      record
         Environment : Command_Environment;
      end record;

   package Command_Vectors is
     new Ada.Containers.Vectors (Positive, Command_Type);

   type Command_Collection is
      record
         Vector : Command_Vectors.Vector;
      end record;

end Chaos.Commands;
