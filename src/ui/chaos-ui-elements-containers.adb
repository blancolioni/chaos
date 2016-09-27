with Ada.Containers.Vectors;
with Ada.Text_IO;

package body Chaos.UI.Elements.Containers is

   package Element_Vectors is
     new Ada.Containers.Vectors (Positive, UI_Element);

   ---------
   -- Add --
   ---------

   procedure Add
     (Container : not null access Root_UI_Container;
      Item      : not null access Chaos.UI.Elements.Root_UI_Element'Class)
   is
   begin
      Item.Parent := UI_Element (Container);
      Container.Children.Append (UI_Element (Item));
   end Add;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Container : in out Root_UI_Container)
   is
   begin
      Container.Children.Clear;
   end Clear;

   -----------
   -- Count --
   -----------

   function Count
     (Container : Root_UI_Container)
      return Natural
   is
   begin
      return Natural (Container.Children.Length);
   end Count;

   -----------------------
   -- Elements_At_Point --
   -----------------------

   overriding function Elements_At_Point
     (Item : not null access Root_UI_Container;
      X, Y : Integer)
      return Array_Of_Elements
   is
      use Css;
      Result : Element_Vectors.Vector;
      Margin_Rec : constant Css.Css_Rectangle :=
                     Css.Layout_Box_Rectangle
                       (Item.Rectangle,
                        Item.Boxes, Css.Margin_Box);
      Css_X : constant Css_Float := Css_Float (X);
      Css_Y : constant Css_Float := Css_Float (Y);
   begin
      if Point_In_Rectangle (Margin_Rec, Css_X, Css_Y) then
         for Child of Item.Children loop
            declare
               Es : constant Array_Of_Elements :=
                      Child.Elements_At_Point
                        (X - Integer (Margin_Rec.X),
                         Y - Integer (Margin_Rec.Y));
            begin
               for E of Es loop
                  Result.Append (E);
               end loop;
            end;
         end loop;
         declare
            Elements : Array_Of_Elements (1 .. Result.Last_Index);
         begin
            for I in Elements'Range loop
               Elements (I) := Result (I);
            end loop;
            return Elements & UI_Element (Item);
         end;
      else
         return No_Elements;
      end if;
   end Elements_At_Point;

   ---------------------
   -- Layout_Children --
   ---------------------

   overriding function Layout_Children
     (Container : Root_UI_Container)
      return Css.Array_Of_Elements
   is
      Result : Css.Array_Of_Elements (1 .. Container.Count);
      Index  : Natural := 0;
   begin
      for Child of Container.Children loop
         if Child.Visible then
            Index := Index + 1;
            Result (Index) := Child;
         end if;
      end loop;
      return Result (1 .. Index);
   end Layout_Children;

   -------------------
   -- New_Container --
   -------------------

   procedure New_Container
     (Container : in out Root_UI_Container'Class;
      Id        : String)
   is
   begin
      Container.Create (Id);
   end New_Container;

   -------------------
   -- New_Container --
   -------------------

   function New_Container
     (Id : String)
      return UI_Container
   is
      Container : constant UI_Container := new Root_UI_Container;
   begin
      Container.New_Container (Id);
      return Container;
   end New_Container;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Container : in out Root_UI_Container;
      Item      : not null access Chaos.UI.Elements.Root_UI_Element'Class)
   is
      use List_Of_Elements;
      Position : Cursor :=
                   Container.Children.Find
                     (UI_Element (Item));
   begin
      if Has_Element (Position) then
         Container.Children.Delete (Position);
      else
         raise Constraint_Error with
           "Container "
           & Css.Css_Element_Interface'Class (Container).Identity
           & " does not contain child "
           & Identity (Item.all);
      end if;
   end Remove;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Element  : not null access Root_UI_Container;
      Renderer : in out Chaos.UI.Renderer.Root_Chaos_Renderer'Class)
   is
      Inner_Rec : constant Css.Css_Rectangle :=
                    Css.Layout_Box_Rectangle
                      (Element.Rectangle, Element.Boxes, Css.Padding_Box);
      DX, DY    : Integer;
   begin
      begin
         DX := Integer (Inner_Rec.X);
         DY := Integer (Inner_Rec.Y);
      exception
         when Constraint_Error =>
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               UI_Element (Element).Layout_Path
               & ": bad rectangle");
            raise;
      end;

      Root_UI_Element (Element.all).Render (Renderer);
      Renderer.Move_Origin (DX, DY);
      for Child of Element.Children loop
         if Child.Visible then
            Child.Render (Renderer);
         end if;
      end loop;
      Renderer.Move_Origin (-DX, -DY);
   end Render;

end Chaos.UI.Elements.Containers;
