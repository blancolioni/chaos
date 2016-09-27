with Cairo;

package Chaos.UI.Gtk_UI.Resources is

   function Image_Surface
     (Name : String)
     return Cairo.Cairo_Surface;

   function Image_Width
     (Name : String)
     return Natural;

   function Image_Height
     (Name : String)
     return Natural;

end Chaos.UI.Gtk_UI.Resources;
