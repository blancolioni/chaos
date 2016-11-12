with Lith.Objects.Interfaces;
with Lith.Objects.Symbols;

with Chaos.Identifiers;

package body Chaos.Creatures.Primitives is

   function Evaluate_Chaos_Get_Reaction
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Chaos_Give_Item
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Chaos_Set_Object_Id
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Is_Creature (Value : Lith.Objects.Object) return Boolean
   is (Chaos.Objects.Is_Object (Value)
       and then Chaos.Objects.To_Object (Value).all in
         Chaos_Creature_Record'Class);

   --------------------
   -- Add_Primitives --
   --------------------

   procedure Add_Primitives is
      use Lith.Objects.Interfaces;
      Creature_Argument : constant Function_Argument_Type :=
                            Custom_Argument (Is_Creature'Access);
      Integer_Or_Symol  : constant Function_Argument_Type :=
                            Integer_Argument or Symbol_Argument;
   begin
      Define_Function ("chaos-get-reaction",
                       Evaluate_Chaos_Get_Reaction'Access);
      Define_Function ("chaos-give-item",
                       Evaluate_Chaos_Give_Item'Access);
      Define_Function ("chaos-set-object-id",
                       (Creature_Argument, Symbol_Argument, Integer_Or_Symol),
                       Evaluate_Chaos_Set_Object_Id'Access);
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

   ----------------------------------
   -- Evaluate_Chaos_Set_Object_Id --
   ----------------------------------

   function Evaluate_Chaos_Set_Object_Id
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      Creature  : constant Chaos.Creatures.Chaos_Creature :=
                    Chaos.Creatures.Chaos_Creature
                      (Chaos.Objects.To_Object (Store.Argument (1)));
      Group_Name : constant String :=
                     Symbols.Get_Name (To_Symbol (Store.Argument (2)));
      New_Value : Natural;

      procedure Set_Object_Id
        (Creature : in out Chaos.Creatures.Chaos_Creature_Record'Class);

      -------------------
      -- Set_Object_Id --
      -------------------

      procedure Set_Object_Id
        (Creature : in out Chaos.Creatures.Chaos_Creature_Record'Class)
      is
      begin
         Creature.Set_Object_Id (Group_Name, New_Value);
      end Set_Object_Id;

   begin
      if Is_Symbol (Store.Argument (3)) then
         New_Value :=
           Chaos.Identifiers.Value
             (Group_Name, Symbols.Get_Name (To_Symbol (Store.Argument (3))));
      else
         New_Value := To_Integer (Store.Argument (3));
      end if;

      Creature.Update (Set_Object_Id'Access);
      return No_Value;
   end Evaluate_Chaos_Set_Object_Id;

end Chaos.Creatures.Primitives;
