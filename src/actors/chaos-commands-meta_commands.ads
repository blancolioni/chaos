with Chaos.Actors;

package Chaos.Commands.Meta_Commands is

   function End_Turn_Command
     (Actor  : Chaos.Actors.Chaos_Actor)
     return Command_Type;

   function Wait_Command
     (Actor  : Chaos.Actors.Chaos_Actor)
     return Command_Type;

end Chaos.Commands.Meta_Commands;
