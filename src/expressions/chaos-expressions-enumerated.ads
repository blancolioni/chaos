generic
   type Enum is (<>);
package Chaos.Expressions.Enumerated is

   function To_Expression (Value : Enum) return Chaos_Expression;
   function Is_Enum (Expression : Chaos_Expression) return Boolean;
   function To_Enum (Expression : Chaos_Expression) return Enum;

   procedure Add_To_Environment
     (Target : in out Chaos_Environment);

end Chaos.Expressions.Enumerated;
