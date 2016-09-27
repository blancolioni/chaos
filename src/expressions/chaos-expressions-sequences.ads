package Chaos.Expressions.Sequences is

   function Sequence_Expression return Chaos_Expression;

   procedure Append
     (To_Sequence : Chaos_Expression;
      Value     : Chaos_Expression);

end Chaos.Expressions.Sequences;
