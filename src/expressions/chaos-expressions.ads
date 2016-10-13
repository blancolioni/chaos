with Lith.Objects;

package Chaos.Expressions is

   procedure Create_Environment;

   function Store return access Lith.Objects.Object_Store'Class;

   function This_Symbol return Lith.Objects.Symbol_Type;

end Chaos.Expressions;
