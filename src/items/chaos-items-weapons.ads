private with Memor;

with Chaos.Dice;

package Chaos.Items.Weapons is

   type Chaos_Weapon_Record is
     new Chaos_Item_Record
   with private;

   function Damage
     (Weapon : Chaos_Weapon_Record'Class)
      return Chaos.Dice.Die_Roll;

   function Finesse (Weapon : Chaos_Weapon_Record'Class) return Boolean;

   type Chaos_Weapon is access constant Chaos_Weapon_Record'Class;

private

   type Chaos_Weapon_Record is new Chaos_Item_Record with
      record
         Damage     : Chaos.Dice.Die_Roll;
         Bonus      : Integer;
         Finesse    : Boolean;
      end record;

   overriding function Object_Database
     (Object : Chaos_Weapon_Record)
      return Memor.Root_Database_Type'Class;

end Chaos.Items.Weapons;
