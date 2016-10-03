package Chaos.Equipment is

   type Chaos_Equipment_Slot is
     (Weapon_1, Weapon_2, Weapon_3, Weapon_4,
      Shield,
      Left_Hand, Right_Hand);

   subtype Weapon_Slot is Chaos_Equipment_Slot range Weapon_1 .. Weapon_4;

   type Chaos_Equippable_Interface is limited interface;

   function Equipment_Slot_OK
     (Equippable : Chaos_Equippable_Interface;
      Slot       : Chaos_Equipment_Slot)
      return Boolean
      is abstract;

end Chaos.Equipment;
