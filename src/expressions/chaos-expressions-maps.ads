package Chaos.Expressions.Maps is

   function Map_Expression return Chaos_Expression;

   function Get
     (Map : Chaos_Expression;
      Key : String)
      return Chaos_Expression;

   procedure Set
     (Map    : Chaos_Expression;
      Key    : String;
      Value  : Chaos_Expression);

   function Is_Map (Expression : Chaos_Expression) return Boolean;

end Chaos.Expressions.Maps;
