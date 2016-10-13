with Lith.Objects.Interfaces;
with Lith.Objects.Symbols;

with Chaos.Creatures;

package body Chaos.Areas.Primitives is

   function Evaluate_Create_Actor
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   -----------------------
   -- Create_Primitives --
   -----------------------

   procedure Create_Primitives is
      use Lith.Objects.Interfaces;
   begin
      Define_Function ("chaos-create-actor", 5, Evaluate_Create_Actor'Access);
   end Create_Primitives;

   ---------------------------
   -- Evaluate_Create_Actor --
   ---------------------------

   function Evaluate_Create_Actor
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Area     : constant Chaos.Areas.Chaos_Area :=
                   Chaos.Areas.Chaos_Area
                     (Chaos.Objects.To_Object
                        (Store.Argument (1)));
      Code     : constant String :=
                   Lith.Objects.Symbols.Get_Name
                     (Lith.Objects.To_Symbol
                        (Store.Argument (2)));
      X        : constant Integer :=
                   Lith.Objects.To_Integer (Store.Argument (3));
      Y        : constant Integer :=
                   Lith.Objects.To_Integer (Store.Argument (4));
      Facing   : constant Integer :=
                   Lith.Objects.To_Integer (Store.Argument (5));
      Creature : constant Chaos.Creatures.Chaos_Creature :=
                   Chaos.Creatures.Get (Code);
      Actor    : constant Chaos.Actors.Chaos_Actor :=
                   Chaos.Actors.Create_Actor
                     (Creature, Area,
                      Area.To_Square ((X, Y)),
                      Chaos.Locations.Orientation'Val
                        (Facing / 2));
   begin
      return Actor.To_Expression;
   end Evaluate_Create_Actor;

end Chaos.Areas.Primitives;
