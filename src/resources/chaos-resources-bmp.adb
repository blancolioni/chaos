package body Chaos.Resources.Bmp is

   -----------
   -- Color --
   -----------

   function Color
     (Resource : Bmp_Resource'Class;
      X, Y     : Natural)
      return Resource_Color
   is
      Result : constant WL.Bitmap_IO.Colour_Type :=
                 WL.Bitmap_IO.Colour (Resource.BM, X, Y);
   begin
      return (WL.Binary_IO.Word_8 (Result.R),
              WL.Binary_IO.Word_8 (Result.G),
              WL.Binary_IO.Word_8 (Result.B),
              WL.Binary_IO.Word_8 (Result.Alpha));

   end Color;

   -----------------
   -- Color_Index --
   -----------------

   function Color_Index
     (Resource : Bmp_Resource'Class;
      X, Y     : Natural)
      return Natural
   is
   begin
      return Natural (WL.Bitmap_IO.Colour_Index (Resource.BM, X, Y));
   end Color_Index;

   ------------
   -- Height --
   ------------

   function Height
     (Resource : Bmp_Resource'Class)
      return Natural
   is
   begin
      return WL.Bitmap_IO.Height (Resource.BM);
   end Height;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Resource : in out Bmp_Resource)
   is
      File : WL.Binary_IO.File_Type :=
               WL.Binary_IO.View (Resource.File, Resource.Start,
                                  Resource.Length);
   begin
      WL.Bitmap_IO.Read (Resource.BM, File);
   end Load;

   -----------
   -- Width --
   -----------

   function Width
     (Resource : Bmp_Resource'Class)
      return Natural
   is
   begin
      return WL.Bitmap_IO.Width (Resource.BM);
   end Width;

end Chaos.Resources.Bmp;
