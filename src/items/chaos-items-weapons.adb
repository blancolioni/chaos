with Chaos.Items.Weapons.Db;

package body Chaos.Items.Weapons is

   --------------
   -- Category --
   --------------

   function Category
     (Weapon : Chaos_Weapon_Record'Class)
      return Weapon_Category
   is
   begin
      return Weapon.Category;
   end Category;

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

   -----------------------
   -- Equipment_Slot_OK --
   -----------------------

   overriding function Equipment_Slot_OK
     (Item : Chaos_Weapon_Record;
      Slot : Chaos.Equipment.Chaos_Equipment_Slot)
      return Boolean
   is
      use Chaos.Equipment;
   begin
      case Slot is
         when Weapon_Slot =>
            return True;
         when Shield =>
            return not Item.Two_Handed;
         when others =>
            return False;
      end case;
   end Equipment_Slot_OK;

   ---------
   -- Get --
   ---------

   function Get (Identifier : String) return Chaos_Weapon is
   begin
      return Db.Get (Identifier);
   end Get;

   -----------
   -- Group --
   -----------

   function Group
     (Weapon : Chaos_Weapon_Record'Class)
      return Weapon_Group
   is
   begin
      return Weapon.Group;
   end Group;

   ------------------
   -- Has_Property --
   ------------------

   function Has_Property
     (Weapon   : Chaos_Weapon_Record'Class;
      Property : Weapon_Property)
      return Boolean
   is
   begin
      return Weapon.Properties (Property);
   end Has_Property;

   ----------------
   -- Long_Range --
   ----------------

   function Long_Range
     (Weapon : Chaos_Weapon_Record'Class)
      return Natural
   is
   begin
      return Weapon.Long_Range;
   end Long_Range;

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

   -----------------------
   -- Proficiency_Bonus --
   -----------------------

   function Proficiency_Bonus
     (Weapon : Chaos_Weapon_Record'Class)
      return Natural
   is
   begin
      return Weapon.Proficiency;
   end Proficiency_Bonus;

   -----------------
   -- Short_Range --
   -----------------

   function Short_Range
     (Weapon : Chaos_Weapon_Record'Class)
      return Natural
   is
   begin
      return Weapon.Short_Range;
   end Short_Range;

end Chaos.Items.Weapons;
