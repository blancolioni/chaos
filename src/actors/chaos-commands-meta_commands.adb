with Chaos.Game;

package body Chaos.Commands.Meta_Commands is

   type Root_Meta_Command is abstract new Root_Command_Type with null record;

   overriding function Group
     (Command : Root_Meta_Command)
      return String
   is ("meta");

   overriding function Class
     (Command : Root_Meta_Command)
      return String
   is ("");

   overriding function Has_Destination
     (Command : Root_Meta_Command)
      return Boolean
   is (False);

   type Root_End_Turn_Command is
     new Root_Meta_Command with
      record
         Turn_Ender : Chaos.Actors.Chaos_Actor;
      end record;

   overriding function Tag
     (Command : Root_End_Turn_Command)
      return String
   is ("end-turn");

   overriding function Available
     (Command : Root_End_Turn_Command)
      return Boolean
   is (True);

   overriding procedure Execute
     (Command  : in out Root_End_Turn_Command);

   type Root_Wait_Command is
     new Root_Meta_Command with
      record
         Waiter : Chaos.Actors.Chaos_Actor;
      end record;

   overriding function Tag
     (Command : Root_Wait_Command)
      return String
   is ("wait");

   overriding function Available
     (Command : Root_Wait_Command)
      return Boolean
   is (True);

   overriding procedure Execute
     (Command  : in out Root_Wait_Command);

   ----------------------
   -- End_Turn_Command --
   ----------------------

   function End_Turn_Command
     (Actor  : Chaos.Actors.Chaos_Actor)
      return Command_Type
   is
   begin
      return new Root_End_Turn_Command'(null, Turn_Ender => Actor);
   end End_Turn_Command;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command  : in out Root_End_Turn_Command)
   is
   begin
      Chaos.Game.Current_Game.Actor_End_Turn (Command.Turn_Ender);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command  : in out Root_Wait_Command)
   is
   begin
      Chaos.Game.Current_Game.Actor_Wait (Command.Waiter);
   end Execute;

   ------------------
   -- Wait_Command --
   ------------------

   function Wait_Command
     (Actor  : Chaos.Actors.Chaos_Actor)
      return Command_Type
   is
   begin
      return new Root_Wait_Command'(null, Waiter => Actor);
   end Wait_Command;

end Chaos.Commands.Meta_Commands;
