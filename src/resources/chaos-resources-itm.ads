package Chaos.Resources.Itm is

   use WL.Binary_IO;

   type Resource_Item_Type is
     (Books, Necklaces, Armor, Belts, Boots, Arrows,
      Gauntlets, Helms, Keys, Potions, Rings,
      Scrolls, Shields, Food, Bullets, Bows,
      Daggers, Maces, Slings, Small_Swords,
      Large_Swords, Hammers, Morning_Stars,
      Flails, Darts, Axes, Quarterstaff,
      Crossbow, Fists, Spears, Halberds,
      Bolts, Clothes, Gold, Gems, Wands,
      Misc_24, Misc_25, Misc_26, Tattoos,
      Lenses, Bucler, Candle, Unkown_2b,
      Clubs, Unknown_2d, Unknown_2e,
      Large_Shield, Unknown_30, Medium_Shield,
      Notes, Unknown_33, Unknown_34, Small_Shield,
      Unknown_36, Telescope, Drink, Great_Sword,
      Container, Fur, Leather_Armor, Studded_Leather_Armor,
      Chain_Mail, Splint_Mail, Half_Plate, Full_Plate,
      Hide_Armor, Robe, Unknown_44, Bastard_Sword,
      Scarf, Food_IWD, Hat, Gauntlet);

   type Item_Resource is new Chaos_Resource with
      record
         Unidentified_Name      : Word_32;
         Identified_Name        : Word_32;
         Replacement_Item       : Resource_Reference;
         Flags                  : Word_32;
         Item_Type              : Resource_Item_Type;
         Usability_Mask         : Word_32;
         Animation              : String (1 .. 2);
         Min_Level              : Word_16;
         Min_Str                : Word_16;
         Min_Str_Bonus          : Word_8;
         Kit_Usability_1        : Word_8;
         Min_Int                : Word_8;
         Kit_Usability_2        : Word_8;
         Min_Dex                : Word_8;
         Kit_Usability_3        : Word_8;
         Min_Wis                : Word_8;
         Kit_Usability_4        : Word_8;
         Min_Con                : Word_8;
         Weapon_Prof            : Word_8;
         Min_Cha                : Word_16;
         Price                  : Word_32;
         Stack_Size             : Word_16;
         Inventory_Icon         : Resource_Reference;
         Lore_To_ID             : Word_16;
         Ground_Icon            : Resource_Reference;
         Weight                 : Word_32;
         Unidentified_Desc      : Word_32;
         Identified_Desc        : Word_32;
         Description_Icon       : Resource_Reference;
         Enchantment            : Word_32;
         Extended_Header_Offset : Word_32;
         Extended_Header_Count  : Word_16;
         Feature_Block_Offset   : Word_32;
         Equipping_Block_Index  : Word_16;
         Equipping_Block_Count  : Word_16;
      end record;

   overriding function Signature
     (Item : Item_Resource)
      return String
   is ("ITM ");

   overriding procedure Load
     (Item : in out Item_Resource);

end Chaos.Resources.Itm;
