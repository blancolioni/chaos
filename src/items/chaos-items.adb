package body Chaos.Items is

   ------------
   -- Weight --
   ------------

   function Weight
     (Item : Chaos_Item_Record)
      return Natural
   is
   begin
      return Item.Weight;
   end Weight;

end Chaos.Items;
