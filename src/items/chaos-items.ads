private with Memor;

with Chaos.Coins;
with Chaos.Weight;

with Chaos.Equipment;
with Chaos.Entities;

with Chaos.Objects;

package Chaos.Items is

   type Chaos_Item_Record is
     new Chaos.Objects.Root_Localised_Object_Record
     and Chaos.Equipment.Chaos_Equippable_Interface
   with private;

   type Chaos_Item is access constant Chaos_Item_Record'Class;

   function Create
     (Entity : not null access constant
        Chaos.Entities.Chaos_Entity_Record'Class)
      return Chaos_Item;

   function Create
     (Entity_Code : String)
      return Chaos_Item;

   function Entity
     (Item : Chaos_Item_Record)
      return Chaos.Entities.Chaos_Entity;

   overriding function Equipment_Slot_OK
     (Item : Chaos_Item_Record;
      Slot : Chaos.Equipment.Chaos_Equipment_Slot)
      return Boolean
   is (Item.Entity.Equipment_Slot_OK (Slot));

   function Weight
     (Item : Chaos_Item_Record)
      return Chaos.Weight.Chaos_Weight
   is (Item.Entity.Weight);

   function Price
     (Item : Chaos_Item_Record)
      return Chaos.Coins.Coins_Type
   is (Item.Entity.Price);

   Inventory_Full : exception;

   type Inventory_Interface is limited interface;

   function Capacity (Inv : Inventory_Interface) return Natural is abstract;
   function Item
     (Inv : Inventory_Interface;
      Index : Positive)
      return Chaos_Item
      is abstract
     with Pre'Class => Index <= Inv.Capacity;

   procedure Replace_Item
     (Inv      : in out Inventory_Interface;
      Index    : Positive;
      New_Item : Chaos_Item)
   is abstract
     with Pre'class => Index <= Inv.Capacity,
       Post'Class => Inv.Item (Index) = New_Item;

   function Has_Item
     (Inv  : Inventory_Interface'Class;
      Item : Chaos_Item)
      return Boolean;

   function Has_Entity
     (Inv    : Inventory_Interface'Class;
      Entity : Chaos.Entities.Chaos_Entity)
      return Boolean;

   function Item
     (Inv    : Inventory_Interface'Class;
      Entity : Chaos.Entities.Chaos_Entity)
      return Chaos_Item;

   procedure Remove_Item
     (Inv  : in out Inventory_Interface'Class;
      Item : Chaos_Item)
     with Pre => Inv.Has_Item (Item);

   procedure Add_Item
     (Inv  : in out Inventory_Interface'Class;
      Item : Chaos_Item);

   procedure Remove_Entity
     (Inv  : in out Inventory_Interface'Class;
      Entity : Chaos.Entities.Chaos_Entity)
     with Pre => Inv.Has_Entity (Entity);

   function Weight
     (Inv : Inventory_Interface'Class)
      return Chaos.Weight.Chaos_Weight;

private

   type Chaos_Item_Record is
     new Chaos.Objects.Root_Localised_Object_Record
     and Chaos.Equipment.Chaos_Equippable_Interface with
      record
         Entity : Chaos.Entities.Chaos_Entity;
      end record;

   overriding function Object_Database
     (Object : Chaos_Item_Record)
      return Memor.Memor_Database;

   overriding procedure Add_Properties
     (Item : Chaos_Item_Record)
   is null;

end Chaos.Items;
