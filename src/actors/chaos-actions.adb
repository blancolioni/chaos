with Chaos.Expressions.Enumerated;

package body Chaos.Actions is

   package Action_Expressions is
     new Chaos.Expressions.Enumerated (Chaos_Action);

   ---------------
   -- Is_Action --
   ---------------

   function Is_Action
     (Expression : Lith.Objects.Object)
      return Boolean
   is
   begin
      return Action_Expressions.Is_Enum (Expression);
   end Is_Action;

   ---------------
   -- To_Action --
   ---------------

   function To_Action
     (Expression : Lith.Objects.Object)
      return Chaos_Action
   is
   begin
      return Action_Expressions.To_Enum (Expression);
   end To_Action;

   -------------------
   -- To_Expression --
   -------------------

   function To_Expression
     (Action : Chaos_Action)
      return Lith.Objects.Object
   is
   begin
      return Action_Expressions.To_Object (Action);
   end To_Expression;

end Chaos.Actions;
