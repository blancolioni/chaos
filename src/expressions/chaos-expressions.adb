with Lith.Environment;
with Lith.Library;
with Lith.Objects.Symbols;

with Chaos.Expressions.Maps;
with Chaos.Expressions.Primitives;

package body Chaos.Expressions is

   ------------------------
   -- Create_Environment --
   ------------------------

   procedure Create_Environment is
   begin
      Lith.Library.Initialise (128 * 1024);
      Lith.Environment.Define
        ("global", Chaos.Expressions.Maps.Create);
      Chaos.Expressions.Primitives.Create_Primitives;
   end Create_Environment;

   -----------
   -- Store --
   -----------

   function Store return access Lith.Objects.Object_Store'Class is
   begin
      return Lith.Library.Store;
   end Store;

   -----------------
   -- This_Symbol --
   -----------------

   function This_Symbol return Lith.Objects.Symbol_Type is
   begin
      return Lith.Objects.Symbols.Get_Symbol ("this");
   end This_Symbol;

end Chaos.Expressions;
