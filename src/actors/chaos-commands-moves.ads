with Chaos.Actors;

package Chaos.Commands.Moves is

   function Move_Command
     (Actor  : Chaos.Actors.Chaos_Actor;
      Environment : not null access constant
        Chaos.Commands.Command_Environment_Interface'Class)
     return Command_Type;

end Chaos.Commands.Moves;
