package Chaos.Expressions.Vectors is

   function Vector_Expression return Chaos_Expression;

   procedure Append
     (To_Vector : Chaos_Expression;
      Value     : Chaos_Expression);

   function Get
     (Vector : Chaos_Expression;
      Index  : Positive)
      return Chaos_Expression;

   procedure Set
     (Vector : Chaos_Expression;
      Index  : Positive;
      Value  : Chaos_Expression);

   function Length (Vector : Chaos_Expression) return Natural;

   function Is_Vector (Expression : Chaos_Expression) return Boolean;

end Chaos.Expressions.Vectors;
