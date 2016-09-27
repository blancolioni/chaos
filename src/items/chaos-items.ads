with Chaos.Objects;

package Chaos.Items is

   type Chaos_Item_Record is
     abstract new Chaos.Objects.Root_Localised_Object_Record with private;

   type Chaos_Item is access constant Chaos_Item_Record'Class;

   function Weight
     (Item : Chaos_Item_Record)
      return Natural;

private

   type Chaos_Item_Record is
     abstract new Chaos.Objects.Root_Localised_Object_Record with
      record
         Weight : Natural;
      end record;

end Chaos.Items;
