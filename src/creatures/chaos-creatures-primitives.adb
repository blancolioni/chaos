with Lith.Objects.Interfaces;

package body Chaos.Creatures.Primitives is

   function Evaluate_Chaos_Get_Reaction
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Chaos_Give_Item
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
      Define_Function ("chaos-give-item", 3,
                       Evaluate_Chaos_Give_Item'Access);
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

   ------------------------------
   -- Evaluate_Chaos_Give_Item --
   ------------------------------

   function Evaluate_Chaos_Give_Item
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Giver : constant Chaos_Creature :=
                Chaos_Creature (Chaos.Objects.To_Object (Store.Argument (1)));
      Entity : constant Chaos.Entities.Chaos_Entity :=
                 Chaos.Entities.Chaos_Entity
                   (Chaos.Objects.To_Object (Store.Argument (2)));
      Item   : constant Chaos.Items.Chaos_Item :=
                 Giver.Item (Entity);
      Receiver : constant Chaos_Creature :=
                   Chaos_Creature
                     (Chaos.Objects.To_Object (Store.Argument (3)));

      procedure Update_Giver
        (Creature : in out Chaos_Creature_Record'Class);

      procedure Update_Receiver
        (Creature : in out Chaos_Creature_Record'Class);

      ------------------
      -- Update_Giver --
      ------------------

      procedure Update_Giver
        (Creature : in out Chaos_Creature_Record'Class)
      is
      begin
         Creature.Remove_Entity (Entity);
      end Update_Giver;

      ---------------------
      -- Update_Receiver --
      ---------------------

      procedure Update_Receiver
        (Creature : in out Chaos_Creature_Record'Class)
      is
      begin
         Creature.Add_Item (Item);
      end Update_Receiver;

   begin
      Giver.Update (Update_Giver'Access);
      Receiver.Update (Update_Receiver'Access);
      return Item.To_Expression;
   end Evaluate_Chaos_Give_Item;

end Chaos.Creatures.Primitives;
