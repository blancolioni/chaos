package Chaos.Expressions.Lists is

   function Cons (Head, Tail : Chaos_Expression) return Chaos_Expression;
   function Head (List : Chaos_Expression) return Chaos_Expression;
   function Tail (List : Chaos_Expression) return Chaos_Expression;

   function Is_List (Expression : Chaos_Expression) return Boolean;

end Chaos.Expressions.Lists;
