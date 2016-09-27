with Css.Layout;
with Chaos.UI.Models.Top_Level;

package body Chaos.UI.Models is

   -------------------
   -- Default_Model --
   -------------------

   function Default_Model return UI_Model is
   begin
      return Chaos.UI.Models.Top_Level.Top_Level_Model;
   end Default_Model;

   -----------------------
   -- Elements_At_Point --
   -----------------------

   overriding function Elements_At_Point
     (Model : not null access Root_UI_Model;
      X, Y  : Integer)
      return Chaos.UI.Elements.Array_Of_Elements
   is
      Top : constant Chaos.UI.Elements.UI_Element :=
              Root_UI_Model'Class (Model.all).Top_Element;
   begin
      return Top.Elements_At_Point (X, Y);
   end Elements_At_Point;

   ----------------
   -- Init_Model --
   ----------------

   procedure Init_Model
     (UI : in out Root_UI_Model'Class;
      Id : String)
   is
   begin
      UI.Create (Id);
   end Init_Model;

   -----------------------
   -- Invalidate_Layout --
   -----------------------

   procedure Invalidate_Layout
     (Model : in out Root_UI_Model)
   is
   begin
      Root_UI_Model'Class (Model).Set_Size
        (Css.Css_Float (Model.Screen_Width),
         Css.Css_Float (Model.Screen_Height));
      Model.Recalculate_Layout := True;
   end Invalidate_Layout;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Model    : not null access Root_UI_Model;
      Renderer : in out Chaos.UI.Renderer.Root_Chaos_Renderer'Class)
   is
      Top : constant Chaos.UI.Elements.UI_Element :=
              Root_UI_Model'Class (Model.all).Top_Element;
   begin
      if Model.Recalculate_Layout then
         Css.Layout.Calculate_Layout (Model, Model.Rectangle, Renderer);
         Model.Recalculate_Layout := False;
      end if;
      Top.Render (Renderer);
   end Render;

   ------------
   -- Resize --
   ------------

   procedure Resize
     (Model      : in out Root_UI_Model;
      New_Width  : Natural;
      New_Height : Natural)
   is
      use Css;
   begin
      if New_Width /= Model.Screen_Width
        or else New_Height /= Model.Screen_Height
      then
         Model.Screen_Width := New_Width;
         Model.Screen_Height := New_Height;
         Root_UI_Model'Class (Model).Set_Size
           (Css.Css_Float (New_Width),
            Css.Css_Float (New_Height));
         Model.Recalculate_Layout := True;
      end if;
   end Resize;

end Chaos.UI.Models;
