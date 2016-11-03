with Lith.Objects.Interfaces;
with Lith.Objects.Symbols;

with Chaos.UI;

with Chaos.Creatures;
with Chaos.Entities.Search;
with Chaos.Items;

package body Chaos.Game.Primitives is

   function Evaluate_Chaos_Party_Experience
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Chaos_Script_Flag
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Chaos_Take_Party_Item
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   --------------------
   -- Add_Primitives --
   --------------------

   procedure Add_Primitives is
      use Lith.Objects.Interfaces;
   begin
      Define_Function ("chaos-party-experience", 1,
                       Evaluate_Chaos_Party_Experience'Access);
      Define_Function ("chaos-script-flag",
                       (Any_Argument_Type, Symbol_Argument, Atom_Argument),
                       Evaluate_Chaos_Script_Flag'Access);
      Define_Function ("chaos-take-party-item", 2,
                       Evaluate_Chaos_Take_Party_Item'Access);
   end Add_Primitives;

   -------------------------------------
   -- Evaluate_Chaos_Party_Experience --
   -------------------------------------

   function Evaluate_Chaos_Party_Experience
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
   begin
      Chaos.UI.Current_UI.Put_Line
        ("The party gains " & Store.Show (Store.Argument (1)) & "XP");

      Current_Game.Party.Give_Experience
        (Lith.Objects.To_Integer (Store.Argument (1)));
      return Store.Argument (1);
   end Evaluate_Chaos_Party_Experience;

   --------------------------------
   -- Evaluate_Chaos_Script_Flag --
   --------------------------------

   function Evaluate_Chaos_Script_Flag
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Map    : constant Lith.Objects.Object :=
                 Store.Argument (1);
      Group  : constant String :=
                 Lith.Objects.Symbols.Get_Name
                   (Lith.Objects.To_Symbol (Store.Argument (2)));
      Name   : constant String :=
                 Store.Show (Store.Argument (3));
      Result : Boolean := False;
   begin
      if Chaos.Objects.Is_Object (Map) then
         Result :=
           Chaos.Game.Current_Game.Script_Flag
             (Chaos.Objects.To_Object (Map), Group, Name);
      else
         raise Constraint_Error with
           "chaos-get-property: expected an object, but found "
           & Store.Show (Map);
      end if;
      return Lith.Objects.To_Object (Result);
   end Evaluate_Chaos_Script_Flag;

   ------------------------------------
   -- Evaluate_Chaos_Take_Party_Item --
   ------------------------------------

   function Evaluate_Chaos_Take_Party_Item
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
--        procedure Take_Item
--          (Creature : in out Chaos.Creatures.Chaos_Creature_Record'Class);
      This   : constant Chaos.Creatures.Chaos_Creature :=
                 Chaos.Creatures.Chaos_Creature
                   (Chaos.Objects.To_Object
                      (Store.Argument (1)));
      Entity : constant Chaos.Entities.Chaos_Entity :=
               Chaos.Entities.Search.Get_Entity
                   (Lith.Objects.Symbols.Get_Name
                      (Lith.Objects.To_Symbol
                         (Store.Argument (2))));
      Item   : constant Chaos.Items.Chaos_Item :=
                 Chaos.Game.Current_Game.Party.Take_Item (Entity);

      procedure Add_Item
        (Creature : in out Chaos.Creatures.Chaos_Creature_Record'Class);

      --------------
      -- Add_Item --
      --------------

      procedure Add_Item
        (Creature : in out Chaos.Creatures.Chaos_Creature_Record'Class)
      is
      begin
         Chaos.UI.Current_UI.Put_Line
           ("The party has lost an item");
         Creature.Add_Item (Item);
      end Add_Item;

   begin
      This.Update (Add_Item'Access);
      return Item.To_Expression;
   end Evaluate_Chaos_Take_Party_Item;

end Chaos.Game.Primitives;
