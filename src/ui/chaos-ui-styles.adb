package body Chaos.UI.Styles is

   Local_Default_Style : aliased Root_Style_Type;

   ---------------
   -- Add_Style --
   ---------------

   procedure Add_Style
     (Selector : String;
      Style    : UI_Style)
   is
      pragma Unreferenced (Selector);
      pragma Unreferenced (Style);
   begin
      null;
   end Add_Style;

   -----------------------
   -- Background_Color --
   -----------------------

   function Background_Color
     (Style : Root_Style_Type'Class)
      return Color_Type
   is
   begin
      return Style.Background_Color;
   end Background_Color;

   -------------------
   -- Border_Color --
   -------------------

   function Border_Color
     (Style : Root_Style_Type'Class)
      return Color_Type
   is
   begin
      return Style.Border_Color;
   end Border_Color;

   -------------------
   -- Border_Radius --
   -------------------

   function Border_Radius
     (Style : Root_Style_Type'Class)
      return Natural
   is
   begin
      return Style.Border_Radius;
   end Border_Radius;

   ------------------
   -- Border_Width --
   ------------------

   function Border_Width
     (Style : Root_Style_Type'Class)
      return Natural
   is
   begin
      return Style.Border_Width;
   end Border_Width;

   -------------------
   -- Default_Style --
   -------------------

   function Default_Style return UI_Style is
      use Ada.Strings.Unbounded;
   begin
      if Local_Default_Style.Font_Family = Null_Unbounded_String then
         Local_Default_Style.Font_Family :=
           To_Unbounded_String ("Tahoma");
      end if;
      return Local_Default_Style'Access;
   end Default_Style;

   -----------------
   -- Font_Family --
   -----------------

   function Font_Family
     (Style : Root_Style_Type'Class)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Style.Font_Family);
   end Font_Family;

   ---------------
   -- Font_Size --
   ---------------

   function Font_Size
     (Style : Root_Style_Type'Class)
      return Positive
   is
   begin
      return Style.Font_Size;
   end Font_Size;

   ----------------
   -- Font_Style --
   ----------------

   function Font_Style
     (Style : Root_Style_Type'Class)
      return Font_Style_Type
   is
   begin
      return Style.Font_Style;
   end Font_Style;

   -----------------
   -- Font_Weight --
   -----------------

   function Font_Weight
     (Style : Root_Style_Type'Class)
      return Font_Weight_Type
   is
   begin
      return Style.Font_Weight;
   end Font_Weight;

   -----------------------
   -- Foreground_Color --
   -----------------------

   function Foreground_Color
     (Style : Root_Style_Type'Class)
      return Color_Type
   is
   begin
      return Style.Foreground_Color;
   end Foreground_Color;

   --------------------------
   -- Horizontal_Alignment --
   --------------------------

   function Horizontal_Alignment
     (Style : Root_Style_Type'Class)
      return Horizontal_Alignment_Type
   is
   begin
      return Style.Horizontal_Alignment;
   end Horizontal_Alignment;

   ------------------
   -- Select_Style --
   ------------------

   function Select_Style
     (Class_Name : String;
      Identifier : String)
      return UI_Style
   is
      pragma Unreferenced (Class_Name);
      pragma Unreferenced (Identifier);
   begin
      return Default_Style;
   end Select_Style;

   ------------------------
   -- Vertical_Alignment --
   ------------------------

   function Vertical_Alignment
     (Style : Root_Style_Type'Class)
      return Vertical_Alignment_Type
   is
   begin
      return Style.Vertical_Alignment;
   end Vertical_Alignment;

end Chaos.UI.Styles;
