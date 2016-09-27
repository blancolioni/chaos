private with Ada.Strings.Unbounded;

with Css;

with Chaos.UI.Events;
with Chaos.UI.Renderer;

package Chaos.UI.Elements is

   type Root_UI_Element is abstract new Css.Css_Element_Interface
   with private;

   overriding function Identity
     (Element : Root_UI_Element)
      return String;

   overriding function State
     (Element : Root_UI_Element)
      return String;

   overriding function Rectangle
     (Element : Root_UI_Element)
      return Css.Css_Rectangle;

   overriding procedure Set_Rectangle
     (Element   : in out Root_UI_Element;
      Rectangle : Css.Css_Rectangle);

   overriding function Box_Layout
     (Element : Root_UI_Element)
      return Css.Layout_Box;

   overriding procedure Set_Box_Layout
     (Element   : in out Root_UI_Element;
      Box       : Css.Layout_Box);

   overriding function Hierarchy
     (Element : Root_UI_Element)
      return String
   is ("Element");

   overriding function Class_Name
     (Element : Root_UI_Element)
      return String
   is ("Element");

   overriding function Text
     (Element : Root_UI_Element)
      return String;

   overriding function Table_Layout
     (Element : Root_UI_Element)
      return Boolean
   is (False);

   overriding function Table_Row_Layout
     (Element : Root_UI_Element)
      return Boolean
   is (False);

   overriding function Table_Rows
     (Element : Root_UI_Element)
      return Css.Array_Of_Elements
   is (Css.No_Elements);

   overriding function Row_Cells
     (Element : Root_UI_Element)
      return Css.Array_Of_Elements
   is (Css.No_Elements);

   procedure Set_Text
     (Element : in out Root_UI_Element;
      Text    : String);

   overriding function Layout_Parent
     (Element : Root_UI_Element)
      return access Css.Css_Element_Interface'Class;

   overriding function Layout_Children
     (Element : Root_UI_Element)
      return Css.Array_Of_Elements
   is (Css.No_Elements);

   procedure Set_Parent
     (Element : in out Root_UI_Element;
      Parent  : access Root_UI_Element'Class);

   overriding function Rule
     (Element : Root_UI_Element)
      return Css.Css_Rule;

   overriding procedure Set_Rule
     (Element : in out Root_UI_Element;
      Rule    : Css.Css_Rule);

   procedure Create
     (Element  : in out Root_UI_Element;
      Identity : String);

   procedure Set_Identity
     (Element  : in out Root_UI_Element;
      Identity : String);

   type Generic_Event_Handler is access
     function (Element : not null access Root_UI_Element'Class;
               Event   : Chaos.UI.Events.UI_Event)
               return Boolean;

   procedure Set_Handler
     (Element   : in out Root_UI_Element'Class;
      For_Event : Chaos.UI.Events.UI_Event_Type;
      Handler   : Generic_Event_Handler);

   function Sensitive
     (Element : Root_UI_Element;
      Event   : Chaos.UI.Events.UI_Event_Type)
      return Boolean
   is (True);

   overriding function Visible
     (Element : Root_UI_Element)
      return Boolean;

   function Enabled
     (Element : Root_UI_Element'Class)
      return Boolean;

   procedure Set_Visible
     (Element : in out Root_UI_Element'Class;
      Value   : Boolean);

   procedure Set_Enabled
     (Element : in out Root_UI_Element'Class;
      Value   : Boolean);

   function On_Event
     (Element : in out Root_UI_Element;
      Event   : Chaos.UI.Events.UI_Event)
      return Boolean;

   procedure Render
     (Element  : not null access Root_UI_Element;
      Renderer : in out Chaos.UI.Renderer.Root_Chaos_Renderer'Class);

   procedure From_Root_Coordinates
     (Element : not null access Root_UI_Element'Class;
      X, Y    : in out Integer);

   type UI_Element is access all Root_UI_Element'Class;

   type Array_Of_Elements is array (Positive range <>) of UI_Element;

   No_Elements : Array_Of_Elements (1 .. 0);

   function Elements_At_Point
     (Element : not null access Root_UI_Element;
      X, Y    : Integer)
      return Array_Of_Elements;

private

   type Event_Handlers is
     array (Chaos.UI.Events.UI_Event_Type) of Generic_Event_Handler;

   type Root_UI_Element is abstract new Css.Css_Element_Interface with
      record
         Identity   : Ada.Strings.Unbounded.Unbounded_String;
         Text       : Ada.Strings.Unbounded.Unbounded_String;
         Parent     : UI_Element;
         Rectangle  : Css.Css_Rectangle := (0.0, 0.0, 100.0, 50.0);
         Boxes      : Css.Layout_Box;
         Handlers   : Event_Handlers   := (others => null);
         Visible    : Boolean := True;
         Enabled    : Boolean := True;
         Rule       : Css.Css_Rule;
         Hover      : Boolean := False;
         Active     : Boolean := False;
      end record;

end Chaos.UI.Elements;
