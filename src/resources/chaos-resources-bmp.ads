private with WL.Bitmap_IO;

package Chaos.Resources.Bmp is

   type Bmp_Resource is new Chaos_Resource with private;

   function Height
     (Resource : Bmp_Resource'Class)
      return Natural;

   function Width
     (Resource : Bmp_Resource'Class)
      return Natural;

   function Color
     (Resource : Bmp_Resource'Class;
      X, Y     : Natural)
      return Resource_Color;

   function Color_Index
     (Resource : Bmp_Resource'Class;
      X, Y     : Natural)
      return Natural;

private

   type Bmp_Resource is new Chaos_Resource with
      record
         BM : WL.Bitmap_IO.Bitmap_Type;
      end record;

   overriding function Signature
     (Resource : Bmp_Resource)
      return String
   is ("");

   overriding procedure Load
     (Resource : in out Bmp_Resource);

end Chaos.Resources.Bmp;
