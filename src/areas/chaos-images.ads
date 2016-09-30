with Chaos.Resources.Tis;

package Chaos.Images is

   type Root_Chaos_Image_Container is abstract tagged private;

   type Chaos_Image_Container is access all Root_Chaos_Image_Container'Class;

   procedure Import_Tileset
     (Container  : in out Root_Chaos_Image_Container;
      Tileset    : Chaos.Resources.Tis.Tis_Resource'Class)
   is abstract;

private

   type Root_Chaos_Image_Container is abstract tagged null record;

end Chaos.Images;
