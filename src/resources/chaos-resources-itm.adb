package body Chaos.Resources.Itm is

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Item : in out Item_Resource)
   is
   begin
      Item.Set_Offset (8);
      Item.Get (Item.Unidentified_Name);
      Item.Get (Item.Identified_Name);
      Item.Get (Item.Replacement_Item);
      Item.Get (Item.Flags);
      declare
         Item_Type : Word_16;
      begin
         Item.Get (Item_Type);
         Item.Item_Type := Resource_Item_Type'Val (Item_Type);
      end;
      Item.Get (Item.Usability_Mask);
      Item.Get (Item.Animation);
      Item.Get (Item.Min_Level);
      Item.Get (Item.Min_Str);
      Item.Get (Item.Min_Str_Bonus);
      Item.Get (Item.Kit_Usability_1);
      Item.Get (Item.Min_Int);
      Item.Get (Item.Kit_Usability_2);
      Item.Get (Item.Min_Dex);
      Item.Get (Item.Kit_Usability_3);
      Item.Get (Item.Min_Wis);
      Item.Get (Item.Kit_Usability_4);
      Item.Get (Item.Min_Con);
      Item.Get (Item.Weapon_Prof);
      Item.Get (Item.Min_Cha);
      Item.Get (Item.Price);
      Item.Get (Item.Stack_Size);
      Item.Get (Item.Inventory_Icon);
      Item.Get (Item.Lore_To_ID);
      Item.Get (Item.Ground_Icon);
      Item.Get (Item.Weight);
      Item.Get (Item.Unidentified_Desc);
      Item.Get (Item.Identified_Desc);
      Item.Get (Item.Description_Icon);
      Item.Get (Item.Enchantment);
      Item.Get (Item.Extended_Header_Offset);
      Item.Get (Item.Extended_Header_Count);
      Item.Get (Item.Feature_Block_Offset);
      Item.Get (Item.Equipping_Block_Index);
      Item.Get (Item.Equipping_Block_Count);

   end Load;

end Chaos.Resources.Itm;
