private with Ada.Strings.Unbounded;

package Chaos.UI.Styles is

   type Horizontal_Alignment_Type is (Left, Middle, Right);
   type Vertical_Alignment_Type is (Top, Middle, Bottom);

   type Color_Type is
      record
         Red, Green, Blue, Alpha : Unit_Float;
      end record;

   Black : constant Color_Type := (0.0, 0.0, 0.0, 1.0);
   White : constant Color_Type := (1.0, 1.0, 1.0, 1.0);
   Transparent : constant Color_Type := (1.0, 1.0, 1.0, 0.0);

   type Font_Weight_Type is (Normal, Bold);
   type Font_Style_Type is (Normal, Italic);

   type Root_Style_Type is tagged private;

   function Horizontal_Alignment
     (Style : Root_Style_Type'Class)
      return Horizontal_Alignment_Type;

   function Vertical_Alignment
     (Style : Root_Style_Type'Class)
      return Vertical_Alignment_Type;

   function Background_Color
     (Style : Root_Style_Type'Class)
      return Color_Type;

   function Foreground_Color
     (Style : Root_Style_Type'Class)
      return Color_Type;

   function Font_Family
     (Style : Root_Style_Type'Class)
      return String;

   function Font_Weight
     (Style : Root_Style_Type'Class)
      return Font_Weight_Type;

   function Font_Style
     (Style : Root_Style_Type'Class)
      return Font_Style_Type;

   function Font_Size
     (Style : Root_Style_Type'Class)
      return Positive;

   function Border_Color
     (Style : Root_Style_Type'Class)
      return Color_Type;

   function Border_Width
     (Style : Root_Style_Type'Class)
      return Natural;

   function Border_Radius
     (Style : Root_Style_Type'Class)
      return Natural;

   type UI_Style is access all Root_Style_Type'Class;

   procedure Add_Style
     (Selector : String;
      Style    : UI_Style);

   function Select_Style
     (Class_Name : String;
      Identifier : String)
      return UI_Style;

   function Default_Style return UI_Style;

private

   type Root_Style_Type is tagged
      record
         Horizontal_Alignment : Horizontal_Alignment_Type := Middle;
         Vertical_Alignment   : Vertical_Alignment_Type   := Middle;
         Foreground_Color    : Color_Type := (1.0, 1.0, 1.0, 1.0);
         Background_Color    : Color_Type := (0.0, 0.0, 0.0, 1.0);
         Font_Family          : Ada.Strings.Unbounded.Unbounded_String;
         Font_Size            : Positive := 12;
         Font_Style           : Font_Style_Type := Normal;
         Font_Weight          : Font_Weight_Type := Normal;
         Border_Color        : Color_Type := White;
         Border_Width         : Natural := 1;
         Border_Radius        : Natural := 0;
      end record;

end Chaos.UI.Styles;
