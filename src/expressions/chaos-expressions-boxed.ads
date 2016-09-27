generic
   type Value_Type is private;
   with function To_String (Value : Value_Type) return String;
--   with function "=" (Left, Right : Value_Type) return Boolean is <>;
Cpackage Chaos.Expressions.Boxed is

   function To_Expression (Value : Value_Type) return Chaos_Expression;
   function Is_Box (Expression : Chaos_Expression) return Boolean;

end Chaos.Expressions.Boxed;
