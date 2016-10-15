private with Memor;

with Chaos.Dice;

package Chaos.Items.Weapons is

   type Weapon_Category is
     (Simple_Melee, Simple_Ranged, Military_Melee, Military_Ranged,
      Superior);

   type Weapon_Group is
     (Mace, Light_Blade, Staff);

   type Weapon_Property is
     (Finesse, Light_Thrown, Off_Hand, Versatile);

   type Chaos_Weapon_Record is
     new Chaos_Item_Record
   with private;

   overriding function Equipment_Slot_OK
     (Item : Chaos_Weapon_Record;
      Slot : Chaos.Equipment.Chaos_Equipment_Slot)
      return Boolean;

   function Category
     (Weapon : Chaos_Weapon_Record'Class)
      return Weapon_Category;

   function Group
     (Weapon : Chaos_Weapon_Record'Class)
      return Weapon_Group;

   function Proficiency_Bonus
     (Weapon : Chaos_Weapon_Record'Class)
      return Natural;

   function Damage
     (Weapon : Chaos_Weapon_Record'Class)
      return Chaos.Dice.Die_Roll;

   function Has_Property
     (Weapon   : Chaos_Weapon_Record'Class;
      Property : Weapon_Property)
      return Boolean;

   function Short_Range
     (Weapon : Chaos_Weapon_Record'Class)
      return Natural;

   function Long_Range
     (Weapon : Chaos_Weapon_Record'Class)
      return Natural;

   type Chaos_Weapon is access constant Chaos_Weapon_Record'Class;

   function Get (Identifier : String) return Chaos_Weapon;
   function Exists (Identifier : String) return Boolean;

private

   type Weapon_Properties is array (Weapon_Property) of Boolean;

   type Chaos_Weapon_Record is new Chaos_Item_Record with
      record
         Category    : Weapon_Category;
         Group       : Weapon_Group;
         Proficiency : Natural := 0;
         Damage      : Chaos.Dice.Die_Roll;
         Two_Handed  : Boolean := False;
         Properties  : Weapon_Properties := (others => False);
         Short_Range : Natural := 0;
         Long_Range  : Natural := 0;
      end record;

   overriding function Object_Database
     (Object : Chaos_Weapon_Record)
      return Memor.Memor_Database;

   overriding procedure Add_Properties
     (Weapon : Chaos_Weapon_Record)
   is null;

end Chaos.Items.Weapons;
