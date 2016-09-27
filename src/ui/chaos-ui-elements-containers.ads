private with Ada.Containers.Doubly_Linked_Lists;

package Chaos.UI.Elements.Containers is

   type Root_UI_Container is new Root_UI_Element with private;

   procedure New_Container
     (Container : in out Root_UI_Container'Class;
      Id        : String);

   procedure Add
     (Container : not null access Root_UI_Container;
      Item      : not null access Chaos.UI.Elements.Root_UI_Element'Class);

   procedure Remove
     (Container : in out Root_UI_Container;
      Item      : not null access Chaos.UI.Elements.Root_UI_Element'Class);

   procedure Clear
     (Container : in out Root_UI_Container);

   function Count
     (Container : Root_UI_Container)
      return Natural;

   overriding function Layout_Children
     (Container : Root_UI_Container)
      return Css.Array_Of_Elements;

   type UI_Container is access all Root_UI_Container'Class;

   function New_Container
     (Id : String)
      return UI_Container;

private

   package List_Of_Elements is
     new Ada.Containers.Doubly_Linked_Lists (UI_Element);

   type Root_UI_Container is new Root_UI_Element with
      record
         Children    : List_Of_Elements.List;
      end record;

   overriding function Class_Name
     (Element : Root_UI_Container)
      return String
   is ("Container");

   overriding function Hierarchy
     (Element : Root_UI_Container)
      return String
   is ("Element Container");

   overriding procedure Render
     (Element  : not null access Root_UI_Container;
      Renderer : in out Chaos.UI.Renderer.Root_Chaos_Renderer'Class);

   overriding function Elements_At_Point
     (Item : not null access Root_UI_Container;
      X, Y : Integer)
      return Array_Of_Elements;

end Chaos.UI.Elements.Containers;
