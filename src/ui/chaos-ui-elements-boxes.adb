package body Chaos.UI.Elements.Boxes is

   procedure Calculate_Layout
     (Box : in out Root_UI_Box'Class);

   ---------
   -- Add --
   ---------

   procedure Add
     (Box : in out Root_UI_Box'Class;
      Item : UI_Element)
   is
   begin
      Box.Children.Append (Item);
   end Add;

   ----------------------
   -- Calculate_Layout --
   ----------------------

   procedure Calculate_Layout
     (Box : in out Root_UI_Box'Class)
   is
      X : Integer := Box.Position.X;
      Y : Integer := Box.Position.Y;
      DX : Natural := 0;
      DY : Natural := 0;
      Child_Size  : Allocation_Type;
      Child_Count : constant Natural :=
                      Natural (Box.Children.Length);
   begin
      if Box.Children.Is_Empty then
         return;
      end if;

      case Box.Orientation is
         when Horizontal =>
            DX := Box.Allocation.Width / Child_Count;
            Child_Size := (DX, Box.Allocation.Height);
         when Vertical =>
            DY := Box.Allocation.Height / Child_Count;
            Child_Size := (Box.Allocation.Width, DY);
      end case;

      for Child of Box.Children loop
         Child.Set_Position ((X, Y));
         Child.Set_Allocation (Child_Size);
         X := X + DX;
         Y := Y + DY;
      end loop;
   end Calculate_Layout;

   overriding procedure Calculate_Size
     (Box      : in out Root_UI_Box;
      Renderer :        Chaos.UI.Renderer.Root_Chaos_Renderer'Class)
   is
      X : Integer := Box.Position.X;
      Y : Integer := Box.Position.Y;
      DX : Natural := 0;
      DY : Natural := 0;
      Child_Size  : Allocation_Type;
      Child_Count : constant Natural :=
                      Natural (Box.Children.Length);
      Max_Width   : Natural := 0;
      Max_Height  : Natural := 0;
   begin
      if Box.Children.Is_Empty then
         Chaos.UI.Elements.Containers.Root_UI_Container (Box).Calculate_Size
           (Renderer);
         return;
      end if;


      case Box.Orientation is
         when Horizontal =>
            DX := Box.Allocation.Width / Child_Count;
            Child_Size := (DX, Box.Allocation.Height);
         when Vertical =>
            DY := Box.Allocation.Height / Child_Count;
            Child_Size := (Box.Allocation.Width, DY);
      end case;

      for Child of Box.Children loop
         Child.Set_Position ((X, Y));
         Child.Set_Allocation (Child_Size);
         X := X + DX;
         Y := Y + DY;
      end loop;
   end Calculate_Size;

   ----------------
   -- Homogenous --
   ----------------

   function Homogenous
     (Box         : in out Root_UI_Box'Class)
      return Boolean
   is
   begin
      return Box.Homogenous;
   end Homogenous;

   -------------
   -- New_Box --
   -------------

   function New_Box
     (Id          : String;
      Orientation : Box_Orientation;
      Homogeneous : Boolean := False)
      return UI_Box
   is
      Result : constant UI_Box := new Root_UI_Box;
   begin
      Result.Create (Id);
      Result.Orientation := Orientation;
      Result.Homogenous  := Homogeneous;
      return Result;
   end New_Box;

   -----------------
   -- Orientation --
   -----------------

   function Orientation
     (Box : Root_UI_Box'Class)
      return Box_Orientation
   is
   begin
      return Box.Orientation;
   end Orientation;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Box      : in out Root_UI_Box;
      Renderer : in out Chaos.UI.Renderer.Root_Chaos_Renderer'Class)
   is
   begin
      Calculate_Layout (Box);
      Root_UI_Element (Box).Render (Renderer);
      for Child of Box.Children loop
         Renderer.Draw_Rectangle
           (Child.Position, Child.Allocation, Box.Style);
         Child.Render (Renderer);
      end loop;
   end Render;

   --------------------
   -- Set_Homogenous --
   --------------------

   procedure Set_Homogenous
     (Box         : in out Root_UI_Box'Class;
      Homogeneous : Boolean)
   is
   begin
      Box.Homogenous := Homogeneous;
   end Set_Homogenous;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
     (Box         : in out Root_UI_Box'Class;
      Orientation : Box_Orientation)
   is
   begin
      Box.Orientation := Orientation;
   end Set_Orientation;

end Chaos.UI.Elements.Boxes;
