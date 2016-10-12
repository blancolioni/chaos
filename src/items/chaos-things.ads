private with Memor;

with Chaos.Coins;
with Chaos.Weight;

with Chaos.Equipment;
with Chaos.Items;

with Chaos.Objects;

package Chaos.Things is

   type Chaos_Thing_Record is
     new Chaos.Objects.Root_Localised_Object_Record
     and Chaos.Equipment.Chaos_Equippable_Interface
   with private;

   type Chaos_Thing is access constant Chaos_Thing_Record'Class;

   function Create
     (Item : not null access constant Chaos.Items.Chaos_Item_Record'Class)
      return Chaos_Thing;

   function Item
     (Thing : Chaos_Thing_Record)
      return Chaos.Items.Chaos_Item;

   overriding function Equipment_Slot_OK
     (Thing : Chaos_Thing_Record;
      Slot : Chaos.Equipment.Chaos_Equipment_Slot)
      return Boolean
   is (Thing.Item.Equipment_Slot_OK (Slot));

   function Weight
     (Thing : Chaos_Thing_Record)
      return Chaos.Weight.Chaos_Weight
   is (Thing.Item.Weight);

   function Price
     (Thing : Chaos_Thing_Record)
      return Chaos.Coins.Coins_Type
   is (Thing.Item.Price);

private

   type Chaos_Thing_Record is
     new Chaos.Objects.Root_Localised_Object_Record
     and Chaos.Equipment.Chaos_Equippable_Interface with
      record
         Item : Chaos.Items.Chaos_Item;
      end record;

   overriding function Object_Database
     (Object : Chaos_Thing_Record)
      return Memor.Memor_Database;

   overriding procedure Add_Properties
     (Thing : Chaos_Thing_Record)
   is null;

end Chaos.Things;
