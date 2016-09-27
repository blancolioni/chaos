package Chaos.Expressions.Numbers is

   function To_Expression (Value : Integer) return Chaos_Expression;
   function Is_Number (Expression : Chaos_Expression) return Boolean;
   function To_Integer (Expression : Chaos_Expression) return Integer;

end Chaos.Expressions.Numbers;
