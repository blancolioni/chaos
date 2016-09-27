with Chaos.UI.Elements;
with Chaos.UI.Renderer;

with Css;

package Chaos.UI.Models is

   type Root_UI_Model is
     abstract new Chaos.UI.Elements.Root_UI_Element with private;

   procedure Init_Model
     (UI : in out Root_UI_Model'Class;
      Id : String);

   overriding procedure Render
     (Model    : not null access Root_UI_Model;
      Renderer : in out Chaos.UI.Renderer.Root_Chaos_Renderer'Class);

   function Top_Element (Model : Root_UI_Model)
                         return Chaos.UI.Elements.UI_Element is abstract;

   overriding function Layout_Children
     (Model : Root_UI_Model)
      return Css.Array_Of_Elements
   is ((1 => Root_UI_Model'Class (Model).Top_Element));

   overriding function Elements_At_Point
     (Model : not null access Root_UI_Model;
      X, Y  : Integer)
      return Chaos.UI.Elements.Array_Of_Elements;

   procedure Invalidate_Layout
     (Model : in out Root_UI_Model);

   procedure Resize
     (Model      : in out Root_UI_Model;
      New_Width  : Natural;
      New_Height : Natural);

   type UI_Model is access all Root_UI_Model'Class;

   function Default_Model return UI_Model;

private

   type Root_UI_Model is
     abstract new Chaos.UI.Elements.Root_UI_Element with
      record
         Recalculate_Layout : Boolean := True;
         Screen_Width       : Natural := 0;
         Screen_Height      : Natural := 0;
      end record;

end Chaos.UI.Models;
