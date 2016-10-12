with Lith.Objects.Interfaces;
with Lith.Objects.Symbols;

with Chaos.Expressions.Maps;

with Chaos.Objects;

with Chaos.Dice;

package body Chaos.Expressions.Primitives is

   function Evaluate_Chaos_Get
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Chaos_Set
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Roll_Dice
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   -----------------------
   -- Create_Primitives --
   -----------------------

   procedure Create_Primitives is
      use Lith.Objects.Interfaces;
   begin
      Define_Function ("chaos-set-property!", 3, Evaluate_Chaos_Set'Access);
      Define_Function ("chaos-get-property", 2, Evaluate_Chaos_Get'Access);
      Define_Function ("chaos-roll-dice", 3, Evaluate_Roll_Dice'Access);
   end Create_Primitives;

   ------------------------
   -- Evaluate_Chaos_Get --
   ------------------------

   function Evaluate_Chaos_Get
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Map   : constant Lith.Objects.Object :=
                Store.Argument (1);
      Name  : constant String :=
                Lith.Objects.Symbols.Get_Name
                  (Lith.Objects.To_Symbol (Store.Argument (2)));
   begin
      if Chaos.Objects.Is_Object (Map) then
         return Chaos.Objects.To_Object (Map).Property (Name);
      elsif Chaos.Expressions.Maps.Is_Map (Map) then
         return Chaos.Expressions.Maps.Get (Map, Name);
      else
         raise Constraint_Error with
           "chaos-get-property: expected a map, but found "
           & Store.Show (Map);
      end if;
   end Evaluate_Chaos_Get;

   ------------------------
   -- Evaluate_Chaos_Set --
   ------------------------

   function Evaluate_Chaos_Set
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Map : constant Lith.Objects.Object :=
              Store.Argument (1);
      Name : constant String :=
               Lith.Objects.Symbols.Get_Name
                 (Lith.Objects.To_Symbol (Store.Argument (2)));
      Value : constant Lith.Objects.Object :=
                Store.Argument (3);
   begin
      if not Chaos.Expressions.Maps.Is_Map (Map) then
         raise Constraint_Error with
           "chaos-setg-property!: expected a map, but found "
           & Store.Show (Map);
      end if;
      Chaos.Expressions.Maps.Set
        (Map, Name, Value);
      return Value;
   end Evaluate_Chaos_Set;

   ------------------------
   -- Evaluate_Roll_Dice --
   ------------------------

   function Evaluate_Roll_Dice
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      Count : constant Natural := To_Integer (Store.Argument (1));
      Die   : constant Natural := To_Integer (Store.Argument (2));
      Plus  : constant Integer := To_Integer (Store.Argument (3));
      Dice : constant Chaos.Dice.Die_Roll :=
                Chaos.Dice.Create (Count, Die, Plus);
   begin
      return To_Object (Chaos.Dice.Roll (Dice));
   end Evaluate_Roll_Dice;

end Chaos.Expressions.Primitives;
