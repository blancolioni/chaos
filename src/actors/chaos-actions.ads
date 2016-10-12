with Lith.Objects;

package Chaos.Actions is

   type Chaos_Action is (Move, Minor, Standard, Free);

   function To_Expression
     (Action : Chaos_Action)
      return Lith.Objects.Object;

   function Is_Action
     (Expression : Lith.Objects.Object)
      return Boolean;

   function To_Action
     (Expression : Lith.Objects.Object)
      return Chaos_Action
     with Pre => Is_Action (Expression);

end Chaos.Actions;
