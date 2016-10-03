with Chaos.Coins;
with Chaos.Weight;

with Chaos.Equipment;
with Chaos.Objects;

package Chaos.Items is

   type Chaos_Item_Record is
     abstract new Chaos.Objects.Root_Localised_Object_Record
     and Chaos.Equipment.Chaos_Equippable_Interface
   with private;

   type Chaos_Item is access constant Chaos_Item_Record'Class;

   overriding function Equipment_Slot_OK
     (Item : Chaos_Item_Record;
      Slot : Chaos.Equipment.Chaos_Equipment_Slot)
      return Boolean
   is (False);

   function Weight
     (Item : Chaos_Item_Record)
      return Chaos.Weight.Chaos_Weight;

   function Price
     (Item : Chaos_Item_Record)
      return Chaos.Coins.Coins_Type;

private

   type Chaos_Item_Record is
     abstract new Chaos.Objects.Root_Localised_Object_Record
     and Chaos.Equipment.Chaos_Equippable_Interface with
      record
         Price  : Chaos.Coins.Coins_Type;
         Weight : Chaos.Weight.Chaos_Weight;
      end record;

end Chaos.Items;
