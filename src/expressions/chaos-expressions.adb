with Lith.Library;

with Chaos.Expressions.Maps;
with Chaos.Expressions.Primitives;

package body Chaos.Expressions is

   ------------------------
   -- Create_Environment --
   ------------------------

   procedure Create_Environment is
   begin
      Lith.Library.Initialise (512 * 1024);
      Store.Define_Top_Level
        (Lith.Objects.Get_Symbol ("global"),
         Chaos.Expressions.Maps.Create);
      Chaos.Expressions.Primitives.Create_Primitives;
   end Create_Environment;

   -----------
   -- Store --
   -----------

   function Store return access Lith.Objects.Object_Store'Class is
   begin
      return Lith.Library.Library_Store;
   end Store;

   -----------------
   -- This_Symbol --
   -----------------

   function This_Symbol return Lith.Objects.Symbol_Type is
   begin
      return Lith.Objects.Get_Symbol ("this");
   end This_Symbol;

end Chaos.Expressions;
