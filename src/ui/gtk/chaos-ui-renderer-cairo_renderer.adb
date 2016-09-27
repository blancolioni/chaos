with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Equal_Case_Insensitive;
with Ada.Strings.Fixed.Hash_Case_Insensitive;

with Ada.Numerics;

with Interfaces.C.Strings;

with Glib;

package body Chaos.UI.Renderer.Cairo_Renderer is

   use Css;

   procedure Draw_Rounded_Rectangle
     (Renderer  : Root_Cairo_Renderer'Class;
      Rectangle : Css.Css_Rectangle;
      Radius    : Css.Css_Float);

   procedure Set_Source_Rgba
     (Renderer : Root_Cairo_Renderer'Class;
      Color    : Css.Css_Color);

   procedure Set_Font
     (Renderer : Root_Cairo_Renderer'Class;
      State    : String;
      Rule     : Css.Css_Rule);

   function To_Cairo_Font_Size
     (Css_Font_Size : Css.Css_Element_Value)
      return Glib.Gdouble;

   package Size_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Glib.Gdouble,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive,
        "="             => Glib."=");

   Font_Size_Name_Map : Size_Maps.Map;

   --------------------
   -- Draw_Rectangle --
   --------------------

   overriding procedure Draw_Rectangle
     (Renderer  : in out Root_Cairo_Renderer;
      Rectangle : Css.Css_Rectangle;
      State     : String;
      Style     : Css.Css_Rule)
   is
      use Glib;
      Border_Radius : constant Css.Css_Float :=
                        Css.To_Measurement
                          (Style.State_Property_Text
                             ("border-radius", State)).Value;
   begin

      if Border_Radius > 0.0 then
         Draw_Rounded_Rectangle
           (Renderer, Rectangle, Border_Radius);
      else
         Cairo.Rectangle
           (Renderer.Cr,
            Gdouble (Rectangle.X) + Gdouble (Renderer.Origin_X),
            Gdouble (Rectangle.Y) + Gdouble (Renderer.Origin_Y),
            Gdouble (Rectangle.W),
            Gdouble (Rectangle.H));
      end if;

      if Style.Border_Width (State) > 0.0 then
         Set_Source_Rgba (Renderer, Style.Border_Color (State));
         Cairo.Set_Line_Width (Renderer.Cr,
                               Width =>
                                 Gdouble (Style.Border_Width (State)));
         Cairo.Stroke_Preserve (Renderer.Cr);
      end if;

      Set_Source_Rgba (Renderer, Style.Background_Color (State));
      Cairo.Fill (Renderer.Cr);

   end Draw_Rectangle;

   ----------------------------
   -- Draw_Rounded_Rectangle --
   ----------------------------

   procedure Draw_Rounded_Rectangle
     (Renderer  : Root_Cairo_Renderer'Class;
      Rectangle : Css.Css_Rectangle;
      Radius    : Css.Css_Float)
   is
      use Glib;
      R  : constant Gdouble := Gdouble (Radius);
      Pi : constant := Ada.Numerics.Pi;
      X1 : constant Gdouble :=
             Gdouble (Renderer.Origin_X)
             + Gdouble (Rectangle.X) + R;
      X2 : constant Gdouble :=
             Gdouble (Renderer.Origin_X)
             + Gdouble (Rectangle.X + Rectangle.W) - R;
      Y1 : constant Gdouble :=
             Gdouble (Renderer.Origin_Y)
             + Gdouble (Rectangle.Y) + R;
      Y2 : constant Gdouble :=
             Gdouble (Renderer.Origin_Y)
             + Gdouble (Rectangle.Y + Rectangle.H) - R;
   begin
      Cairo.New_Path (Renderer.Cr);
      Cairo.Arc (Renderer.Cr, X1, Y1, R, 2.0 * Pi / 2.0, 3.0 * Pi / 2.0);
      Cairo.Arc (Renderer.Cr, X2, Y1, R, 3.0 * Pi / 2.0, 4.0 * Pi / 2.0);
      Cairo.Arc (Renderer.Cr, X2, Y2, R, 0.0 * Pi / 2.0, 1.0 * Pi / 2.0);
      Cairo.Arc (Renderer.Cr, X1, Y2, R, 1.0 * Pi / 2.0, 2.0 * Pi / 2.0);
      Cairo.Close_Path (Renderer.Cr);
   end Draw_Rounded_Rectangle;

   ---------------
   -- Draw_Text --
   ---------------

   overriding procedure Draw_Text
     (Renderer  : in out Root_Cairo_Renderer;
      Rectangle : Css.Css_Rectangle;
      State     : String;
      Style     : Css.Css_Rule;
      Text     : String)
   is
      use Glib;
      X, Y : Gdouble;
   begin
      Set_Source_Rgba (Renderer, Style.Foreground_Color (State));
      Set_Font (Renderer, State, Style);

      X := Gdouble (Renderer.Origin_X) + Gdouble (Rectangle.X);
      Y := Gdouble (Renderer.Origin_Y) + Gdouble (Rectangle.Y)
        + Gdouble (Rectangle.H);

      Cairo.Move_To
        (Renderer.Cr, X, Y);

      Cairo.Show_Text (Renderer.Cr, Text);

   end Draw_Text;

   ----------
   -- Init --
   ----------

   procedure Init
     (Renderer : in out Root_Cairo_Renderer'Class;
      Context  : Cairo.Cairo_Context)
   is
   begin
      Renderer.Cr := Context;
   end Init;

   -------------------------
   -- Measure_Text_Height --
   -------------------------

   overriding function Measure_Text_Height
     (Renderer         : Root_Cairo_Renderer;
      State            : String;
      Style            : Css.Css_Rule;
      Available_Width  : Css.Css_Float;
      Text             : String)
      return Css.Css_Float
   is
      Extents : aliased Cairo.Cairo_Text_Extents;
      Str     : Interfaces.C.Strings.chars_ptr;
      Lines   : Positive;
      Width   : Css_Float;
   begin

      if Text = "" then
         return 0.0;
      end if;

      Set_Font (Renderer, State, Style);
      Str := Interfaces.C.Strings.New_String (Text);
      Cairo.Text_Extents (Renderer.Cr, Str, Extents'Access);

      Width := Css_Float (Extents.Width);
      if Width <= Available_Width then
         Lines := 1;
      else
         Lines := Positive (Width / Available_Width);
      end if;

      Interfaces.C.Strings.Free (Str);

      return Css_Float (Extents.Height) * Css_Float (Lines);
   end Measure_Text_Height;

   ------------------------
   -- Measure_Text_Width --
   ------------------------

   overriding function Measure_Text_Width
     (Renderer         : Root_Cairo_Renderer;
      State            : String;
      Style            : Css.Css_Rule;
      Available_Height : Css.Css_Float;
      Text             : String)
      return Css.Css_Float
   is
      Extents : aliased Cairo.Cairo_Text_Extents;
      Str     : Interfaces.C.Strings.chars_ptr;
      Lines   : Positive;
      Height  : Css_Float;
   begin

      if Text = "" then
         return 0.0;
      end if;

      Set_Font (Renderer, State, Style);
      Str := Interfaces.C.Strings.New_String (Text);
      Cairo.Text_Extents (Renderer.Cr, Str, Extents'Access);

      Height := Css_Float (Extents.Height);
      if Height > Available_Height then
         Lines := 1;
      else
         Lines := Positive (Available_Height / Height);
      end if;

      Interfaces.C.Strings.Free (Str);

      return Css_Float (Extents.Width) / Css_Float (Lines);

   end Measure_Text_Width;

   --------------
   -- Set_Font --
   --------------

   procedure Set_Font
     (Renderer : Root_Cairo_Renderer'Class;
      State    : String;
      Rule     : Css.Css_Rule)
   is
      Slant : Cairo.Cairo_Font_Slant :=
                Cairo.Cairo_Font_Slant_Normal;
      Weight : Cairo.Cairo_Font_Weight :=
                 Cairo.Cairo_Font_Weight_Normal;
      Css_Style : constant String :=
                    Rule.State_Property_Text ("font-style", State);
      Css_Weight : constant String :=
                     Rule.State_Property_Text ("font-weight", State);
   begin

      if Css_Style = "oblique" then
         Slant := Cairo.Cairo_Font_Slant_Oblique;
      elsif Css_Style = "italic" then
         Slant := Cairo.Cairo_Font_Slant_Italic;
      end if;

      if Css_Weight = "bold" then
         Weight := Cairo.Cairo_Font_Weight_Bold;
      end if;

      Cairo.Select_Font_Face
        (Cr     => Renderer.Cr,
         Family => Rule.State_Property_Text ("font-family", State),
         Slant  => Slant,
         Weight => Weight);

      Cairo.Set_Font_Size
        (Renderer.Cr,
         To_Cairo_Font_Size (Rule.State_Property_Value ("font-size", State)));
   end Set_Font;

   ---------------------
   -- Set_Source_Rgba --
   ---------------------

   procedure Set_Source_Rgba
     (Renderer : Root_Cairo_Renderer'Class;
      Color    : Css.Css_Color)
   is
      use Glib;
   begin
      Cairo.Set_Source_Rgba
        (Renderer.Cr,
         Gdouble (Color.Red),
         Gdouble (Color.Green),
         Gdouble (Color.Blue),
         Gdouble (Color.Alpha));
   end Set_Source_Rgba;

   ------------------------
   -- To_Cairo_Font_Size --
   ------------------------

   function To_Cairo_Font_Size
     (Css_Font_Size : Css.Css_Element_Value)
      return Glib.Gdouble
   is
   begin
      if Font_Size_Name_Map.Is_Empty then
         Font_Size_Name_Map.Insert ("xx-small", 6.0);
         Font_Size_Name_Map.Insert ("x-small", 10.0);
         Font_Size_Name_Map.Insert ("small", 12.0);
         Font_Size_Name_Map.Insert ("medium", 16.0);
         Font_Size_Name_Map.Insert ("large", 24.0);
         Font_Size_Name_Map.Insert ("x-large", 36.0);
         Font_Size_Name_Map.Insert ("xx-large", 48.0);
      end if;

      if Css.Is_String (Css_Font_Size) then
         declare
            Name : constant String :=
                     Css.To_String (Css_Font_Size);
         begin
            if Font_Size_Name_Map.Contains (Name) then
               return Font_Size_Name_Map.Element (Name);
            else
               return Glib.Gdouble (Css.To_Measurement (Name).Value);
            end if;
         end;
      end if;

      return Font_Size_Name_Map ("medium");

   end To_Cairo_Font_Size;

end Chaos.UI.Renderer.Cairo_Renderer;
