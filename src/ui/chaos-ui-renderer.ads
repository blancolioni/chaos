with Css;

package Chaos.UI.Renderer is

   type Root_Chaos_Renderer is
     abstract new Css.Css_Render_Interface with private;

   procedure Set_Origin
     (Renderer : in out Root_Chaos_Renderer;
      X, Y     : Integer);

   procedure Move_Origin
     (Renderer : in out Root_Chaos_Renderer;
      DX, DY   : Integer);

   procedure Draw_Rectangle
     (Renderer  : in out Root_Chaos_Renderer;
      Rectangle : Css.Css_Rectangle;
      State     : String;
      Style     : Css.Css_Rule)
   is abstract;

   procedure Draw_Text
     (Renderer  : in out Root_Chaos_Renderer;
      Rectangle : Css.Css_Rectangle;
      State     : String;
      Style     : Css.Css_Rule;
      Text      : String)
   is abstract;

private

   type Root_Chaos_Renderer is
     abstract new Css.Css_Render_Interface with
      record
         Origin_X : Integer;
         Origin_Y : Integer;
      end record;

end Chaos.UI.Renderer;
