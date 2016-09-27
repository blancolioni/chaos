with Chaos.Expressions;

package Chaos.Actions is

   type Chaos_Action is (Move, Minor, Standard, Free);

   function To_Expression
     (Action : Chaos_Action)
      return Chaos.Expressions.Chaos_Expression;

   function Is_Action
     (Expression : Chaos.Expressions.Chaos_Expression)
      return Boolean;

   function To_Action
     (Expression : Chaos.Expressions.Chaos_Expression)
      return Chaos_Action;

end Chaos.Actions;
