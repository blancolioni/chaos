private with Ada.Containers.Doubly_Linked_Lists;
with Chaos.UI.Elements.Containers;

package Chaos.UI.Elements.Boxes is

   type Box_Orientation is (Horizontal, Vertical);

   type Root_UI_Box is
     new Chaos.UI.Elements.Containers.Root_UI_Container with private;

   type UI_Box is access all Root_UI_Box'Class;

   function New_Box
     (Id          : String;
      Orientation : Box_Orientation;
      Homogeneous : Boolean := False)
      return UI_Box;

   function Orientation
     (Box : Root_UI_Box'Class)
      return Box_Orientation;

   procedure Set_Orientation
     (Box         : in out Root_UI_Box'Class;
      Orientation : Box_Orientation);

   procedure Set_Homogenous
     (Box         : in out Root_UI_Box'Class;
      Homogeneous : Boolean);

   function Homogenous
     (Box         : in out Root_UI_Box'Class)
      return Boolean;

   overriding procedure Add
     (Box : in out Root_UI_Box;
      Item : UI_Element);

private

   type Root_UI_Box is
     new Chaos.UI.Elements.Containers.Root_UI_Container with
      record
         Orientation : Box_Orientation;
         Homogenous  : Boolean;
      end record;

   overriding procedure Calculate_Size
     (Box      : in out Root_UI_Box;
      Renderer :        Chaos.UI.Renderer.Root_Chaos_Renderer'Class);

   overriding procedure Render
     (Box      : in out Root_UI_Box;
      Renderer : in out Chaos.UI.Renderer.Root_Chaos_Renderer'Class);

end Chaos.UI.Elements.Boxes;
