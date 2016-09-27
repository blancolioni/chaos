--  with Ada.Text_IO;

with Chaos.UI.Main;

package body Chaos.UI.Elements is

   function On_Button_Press
     (Element : not null access Root_UI_Element'Class;
      Event   : Chaos.UI.Events.UI_Event)
      return Boolean;

   function On_Button_Release
     (Element : not null access Root_UI_Element'Class;
      Event   : Chaos.UI.Events.UI_Event)
      return Boolean;

   function On_Mouse_Enter
     (Element : not null access Root_UI_Element'Class;
      Event   : Chaos.UI.Events.UI_Event)
      return Boolean;

   function On_Mouse_Leave
     (Element : not null access Root_UI_Element'Class;
      Event   : Chaos.UI.Events.UI_Event)
      return Boolean;

   ----------------
   -- Box_Layout --
   ----------------

   overriding function Box_Layout
     (Element : Root_UI_Element)
      return Css.Layout_Box
   is
   begin
      return Element.Boxes;
   end Box_Layout;

   ------------
   -- Create --
   ------------

   procedure Create
     (Element  : in out Root_UI_Element;
      Identity : String)
   is
   begin
      Element.Identity :=
        Ada.Strings.Unbounded.To_Unbounded_String (Identity);
      Element.Rule := Css.Root_Rule;
      Element.Set_Handler (Chaos.UI.Events.Button_Press,
                           On_Button_Press'Access);
      Element.Set_Handler (Chaos.UI.Events.Button_Release,
                           On_Button_Release'Access);
      Element.Set_Handler (Chaos.UI.Events.Mouse_Enter,
                           On_Mouse_Enter'Access);
      Element.Set_Handler (Chaos.UI.Events.Mouse_Leave,
                           On_Mouse_Leave'Access);
   end Create;

   -----------------------
   -- Elements_At_Point --
   -----------------------

   function Elements_At_Point
     (Element : not null access Root_UI_Element;
      X, Y    : Integer)
      return Array_Of_Elements
   is
      use Css;
      Margin_Rec : constant Css.Css_Rectangle :=
                     Css.Layout_Box_Rectangle
                       (Element.Rectangle,
                        Element.Boxes, Css.Margin_Box);
      Css_X : constant Css_Float := Css_Float (X);
      Css_Y : constant Css_Float := Css_Float (Y);
   begin
      if Point_In_Rectangle (Margin_Rec, Css_X, Css_Y) then
         return (1 => UI_Element (Element));
      else
         return No_Elements;
      end if;
   end Elements_At_Point;

   -------------
   -- Enabled --
   -------------

   function Enabled
     (Element : Root_UI_Element'Class)
      return Boolean
   is
   begin
      return Element.Enabled;
   end Enabled;

   ---------------------------
   -- From_Root_Coordinates --
   ---------------------------

   procedure From_Root_Coordinates
     (Element : not null access Root_UI_Element'Class;
      X, Y    : in out Integer)
   is
   begin
      if Element.Parent /= null then
         X := X - Integer (Element.Rectangle.X);
         Y := Y - Integer (Element.Rectangle.Y);
         Element.Parent.From_Root_Coordinates (X, Y);
      end if;
   end From_Root_Coordinates;

   --------------
   -- Identity --
   --------------

   overriding function Identity
     (Element : Root_UI_Element)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Element.Identity);
   end Identity;

   -------------------
   -- Layout_Parent --
   -------------------

   overriding function Layout_Parent
     (Element : Root_UI_Element)
      return access Css.Css_Element_Interface'Class
   is
   begin
      return Element.Parent;
   end Layout_Parent;

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press
     (Element : not null access Root_UI_Element'Class;
      Event   : Chaos.UI.Events.UI_Event)
      return Boolean
   is
      pragma Unreferenced (Event);
   begin
      Element.Active := True;
      Chaos.UI.Main.Current_UI.Invalidate;
      return True;
   end On_Button_Press;

   -----------------------
   -- On_Button_Release --
   -----------------------

   function On_Button_Release
     (Element : not null access Root_UI_Element'Class;
      Event   : Chaos.UI.Events.UI_Event)
      return Boolean
   is
      pragma Unreferenced (Event);
   begin
      Element.Active := False;
      Chaos.UI.Main.Current_UI.Invalidate;
      return True;
   end On_Button_Release;

   --------------
   -- On_Event --
   --------------

   function On_Event
     (Element : in out Root_UI_Element;
      Event   : Chaos.UI.Events.UI_Event)
      return Boolean
   is
   begin
      if Element.Handlers (Event.Event_Type) /= null then
         return Element.Handlers (Event.Event_Type) (Element'Access, Event);
      end if;
      return False;
   end On_Event;

   --------------------
   -- On_Mouse_Enter --
   --------------------

   function On_Mouse_Enter
     (Element : not null access Root_UI_Element'Class;
      Event   : Chaos.UI.Events.UI_Event)
      return Boolean
   is
      pragma Unreferenced (Event);
   begin
--        Ada.Text_IO.Put_Line
--          ("Enter: " & Identity (Element.all));
      Element.Hover := True;
      Chaos.UI.Main.Current_UI.Invalidate;
      return True;
   end On_Mouse_Enter;

   --------------------
   -- On_Mouse_Leave --
   --------------------

   function On_Mouse_Leave
     (Element : not null access Root_UI_Element'Class;
      Event   : Chaos.UI.Events.UI_Event)
      return Boolean
   is
      pragma Unreferenced (Event);
   begin
--        Ada.Text_IO.Put_Line
--          ("Leave: " & Identity (Element.all));
      Element.Hover := False;
      Element.Active := False;
      Chaos.UI.Main.Current_UI.Invalidate;
      return True;
   end On_Mouse_Leave;

   ---------------
   -- Rectangle --
   ---------------

   overriding function Rectangle
     (Element : Root_UI_Element)
      return Css.Css_Rectangle
   is
   begin
      return Element.Rectangle;
   end Rectangle;

   ------------
   -- Render --
   ------------

   procedure Render
     (Element  : not null access Root_UI_Element;
      Renderer : in out Chaos.UI.Renderer.Root_Chaos_Renderer'Class)
   is
      use Ada.Strings.Unbounded;
   begin

--        Ada.Text_IO.Put_Line
--          (Identity (Element)
--           & ": base: "
--           & Css.Show (Element.Rectangle));
--        Ada.Text_IO.Put_Line
--          (Identity (Element)
--           & ": margin: "
--           & Css.Show
--             (Css.Layout_Box_Rectangle
--                  (Element.Rectangle, Element.Boxes, Css.Margin_Box)));
--        Ada.Text_IO.Put_Line
--          (Identity (Element)
--           & ": border: "
--           & Css.Show
--             (Css.Layout_Box_Rectangle
--                  (Element.Rectangle, Element.Boxes, Css.Border_Box)));
--        Ada.Text_IO.Put_Line
--          (Identity (Element)
--           & ": padding: "
--           & Css.Show
--             (Css.Layout_Box_Rectangle
--                  (Element.Rectangle, Element.Boxes, Css.Padding_Box)));

      Renderer.Draw_Rectangle
        (Css.Layout_Box_Rectangle
           (Element.Rectangle, Element.Boxes, Css.Margin_Box),
         Element.State, Element.Rule);

      if Element.Text /= Null_Unbounded_String then
         Renderer.Draw_Text
           (Css.Layout_Box_Rectangle
              (Element.Rectangle, Element.Boxes, Css.Padding_Box),
            Element.State, Element.Rule, To_String (Element.Text));
      end if;

   end Render;

   ----------
   -- Rule --
   ----------

   overriding function Rule
     (Element : Root_UI_Element)
      return Css.Css_Rule
   is
   begin
      return Element.Rule;
   end Rule;

   --------------------
   -- Set_Box_Layout --
   --------------------

   overriding procedure Set_Box_Layout
     (Element   : in out Root_UI_Element;
      Box       : Css.Layout_Box)
   is
   begin
      Element.Boxes := Box;
   end Set_Box_Layout;

   -----------------
   -- Set_Enabled --
   -----------------

   procedure Set_Enabled
     (Element : in out Root_UI_Element'Class;
      Value   : Boolean)
   is
   begin
      Element.Enabled := Value;
      Chaos.UI.Main.Current_UI.Invalidate;
   end Set_Enabled;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (Element   : in out Root_UI_Element'Class;
      For_Event : Chaos.UI.Events.UI_Event_Type;
      Handler   : Generic_Event_Handler)
   is
   begin
      Element.Handlers (For_Event) := Handler;
   end Set_Handler;

   ------------------
   -- Set_Identity --
   ------------------

   procedure Set_Identity
     (Element  : in out Root_UI_Element;
      Identity : String)
   is
   begin
      Element.Identity := Ada.Strings.Unbounded.To_Unbounded_String (Identity);
   end Set_Identity;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent
     (Element : in out Root_UI_Element;
      Parent  : access Root_UI_Element'Class)
   is
   begin
      Element.Parent := UI_Element (Parent);
   end Set_Parent;

   -------------------
   -- Set_Rectangle --
   -------------------

   overriding procedure Set_Rectangle
     (Element   : in out Root_UI_Element;
      Rectangle : Css.Css_Rectangle)
   is
   begin
--        Ada.Text_IO.Put_Line
--          (Identity (Root_UI_Element'Class (Element))
--           & ": new rect = "
--           & Css.Show (Rectangle));

      Element.Rectangle := Rectangle;
   end Set_Rectangle;

   --------------
   -- Set_Rule --
   --------------

   overriding procedure Set_Rule
     (Element : in out Root_UI_Element;
      Rule    : Css.Css_Rule)
   is
   begin
      Element.Rule := Rule;
   end Set_Rule;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Element : in out Root_UI_Element;
      Text    : String)
   is
   begin
      Element.Text := Ada.Strings.Unbounded.To_Unbounded_String (Text);
      Chaos.UI.Main.Current_UI.Invalidate;
   end Set_Text;

   -----------------
   -- Set_Visible --
   -----------------

   procedure Set_Visible
     (Element : in out Root_UI_Element'Class;
      Value   : Boolean)
   is
   begin
      Element.Visible := Value;
      Chaos.UI.Main.Current_UI.Invalidate (Layout_Changed => True);
   end Set_Visible;

   -----------
   -- State --
   -----------

   overriding function State
     (Element : Root_UI_Element)
      return String
   is
   begin
      if not Element.Enabled then
         return "insensitive";
      elsif Element.Active then
         return "active";
      elsif Element.Hover then
         return "hover";
      else
         return "";
      end if;
   end State;

   ----------
   -- Text --
   ----------

   overriding function Text
     (Element : Root_UI_Element)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Element.Text);
   end Text;

   -------------
   -- Visible --
   -------------

   overriding function Visible
     (Element : Root_UI_Element)
      return Boolean
   is
   begin
      return Element.Visible;
   end Visible;

end Chaos.UI.Elements;
