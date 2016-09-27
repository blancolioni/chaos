with Cairo;

package Chaos.UI.Renderer.Cairo_Renderer is

   type Root_Cairo_Renderer is
     new Root_Chaos_Renderer with private;

   procedure Init
     (Renderer : in out Root_Cairo_Renderer'Class;
      Context  : Cairo.Cairo_Context);

private

   type Root_Cairo_Renderer is
     new Root_Chaos_Renderer with
      record
         Cr : Cairo.Cairo_Context;
      end record;

   overriding procedure Draw_Rectangle
     (Renderer  : in out Root_Cairo_Renderer;
      Rectangle : Css.Css_Rectangle;
      State     : String;
      Style     : Css.Css_Rule);

   overriding procedure Draw_Text
     (Renderer  : in out Root_Cairo_Renderer;
      Rectangle : Css.Css_Rectangle;
      State     : String;
      Style     : Css.Css_Rule;
      Text      : String);

   overriding function Measure_Text_Width
     (Renderer         : Root_Cairo_Renderer;
      State            : String;
      Style            : Css.Css_Rule;
      Available_Height : Css.Css_Float;
      Text             : String)
      return Css.Css_Float;

   overriding function Measure_Text_Height
     (Renderer         : Root_Cairo_Renderer;
      State            : String;
      Style            : Css.Css_Rule;
      Available_Width  : Css.Css_Float;
      Text             : String)
      return Css.Css_Float;

end Chaos.UI.Renderer.Cairo_Renderer;
