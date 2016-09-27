package body Chaos.UI.Renderer is

   -----------------
   -- Move_Origin --
   -----------------

   procedure Move_Origin
     (Renderer : in out Root_Chaos_Renderer;
      DX, DY   : Integer)
   is
   begin
      Renderer.Origin_X := Renderer.Origin_X + DX;
      Renderer.Origin_Y := Renderer.Origin_Y + DY;
   end Move_Origin;

   ----------------
   -- Set_Origin --
   ----------------

   procedure Set_Origin
     (Renderer : in out Root_Chaos_Renderer;
      X, Y     : Integer)
   is
   begin
      Renderer.Origin_X := X;
      Renderer.Origin_Y := Y;
   end Set_Origin;

end Chaos.UI.Renderer;
