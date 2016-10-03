package body Chaos.Items is

   -----------
   -- Price --
   -----------

   function Price
     (Item : Chaos_Item_Record)
      return Chaos.Coins.Coins_Type
   is
   begin
      return Item.Price;
   end Price;

   ------------
   -- Weight --
   ------------

   function Weight
     (Item : Chaos_Item_Record)
      return Chaos.Weight.Chaos_Weight
   is
   begin
      return Item.Weight;
   end Weight;

end Chaos.Items;
