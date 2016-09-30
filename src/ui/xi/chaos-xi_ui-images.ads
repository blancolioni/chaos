private with Ada.Containers.Indefinite_Vectors;

with Xi.Color;

with Chaos.Resources.Tis;

with Chaos.Images;

package Chaos.Xi_UI.Images is

   type Xi_Image_Container_Record is
     new Chaos.Images.Root_Chaos_Image_Container with private;

   type Xi_Image_Container is access all Xi_Image_Container_Record'Class;

   overriding procedure Import_Tileset
     (Container  : in out Xi_Image_Container_Record;
      Tileset    : Chaos.Resources.Tis.Tis_Resource'Class);

   function Tile
     (Container  : Xi_Image_Container_Record'Class;
      Index      : Positive)
      return Xi.Color.Xi_Color_2D_Array;

private

   package Tile_Image_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Positive, Xi.Color.Xi_Color_2D_Array,
        Xi.Color."=");

   type Xi_Image_Container_Record is
     new Chaos.Images.Root_Chaos_Image_Container with
      record
         Tile_Images : Tile_Image_Vectors.Vector;
      end record;

end Chaos.Xi_UI.Images;
