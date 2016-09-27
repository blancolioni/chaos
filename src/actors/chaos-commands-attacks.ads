with Chaos.Actors;
with Chaos.Powers;

package Chaos.Commands.Attacks is

   function Power_Attack
     (Actor       : Chaos.Actors.Chaos_Actor;
      Power       : Chaos.Powers.Chaos_Power;
      Environment : not null access constant
        Chaos.Commands.Command_Environment_Interface'Class)
      return Command_Type;

end Chaos.Commands.Attacks;
