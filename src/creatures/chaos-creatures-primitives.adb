with Lith.Objects.Interfaces;

package body Chaos.Creatures.Primitives is

   function Evaluate_Chaos_Get_Reaction
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   --------------------
   -- Add_Primitives --
   --------------------

   procedure Add_Primitives is
      use Lith.Objects.Interfaces;
   begin
      Define_Function ("chaos-get-reaction", 2,
                       Evaluate_Chaos_Get_Reaction'Access);
   end Add_Primitives;

   ---------------------------------
   -- Evaluate_Chaos_Get_Reaction --
   ---------------------------------

   function Evaluate_Chaos_Get_Reaction
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
   begin
      return Lith.Objects.To_Object
        (Chaos_Creature (Chaos.Objects.To_Object (Store.Argument (1)))
         .Get_Reaction
           (Chaos_Creature (Chaos.Objects.To_Object (Store.Argument (2)))));
   end Evaluate_Chaos_Get_Reaction;

end Chaos.Creatures.Primitives;
