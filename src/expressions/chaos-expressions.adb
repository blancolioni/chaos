with Lith.Environment;
with Lith.Library;

with Chaos.Expressions.Maps;
with Chaos.Expressions.Primitives;

package body Chaos.Expressions is

   ------------------------
   -- Create_Environment --
   ------------------------

   procedure Create_Environment is
   begin
      Lith.Library.Initialise (256 * 1024);
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

end Chaos.Expressions;
