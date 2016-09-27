with Chaos.Items.Weapons.Db;

package body Chaos.Items.Weapons is

   ------------
   -- Damage --
   ------------

   function Damage
     (Weapon : Chaos_Weapon_Record'Class)
      return Chaos.Dice.Die_Roll
   is
   begin
      return Weapon.Damage;
   end Damage;

   -------------
   -- Finesse --
   -------------

   function Finesse (Weapon : Chaos_Weapon_Record'Class) return Boolean is
   begin
      return Weapon.Finesse;
   end Finesse;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Object : Chaos_Weapon_Record)
      return Memor.Root_Database_Type'Class
   is
      pragma Unreferenced (Object);
   begin
      return Db.Get_Database;
   end Object_Database;

end Chaos.Items.Weapons;
