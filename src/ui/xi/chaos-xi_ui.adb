with Ada.Text_IO;

with GL;

with Xi.Main;
with Xi.Render_Window;
with Xi.Scene;

with Chaos.Game;
with Chaos.Images;

with Chaos.Xi_UI.Models;
with Chaos.Xi_UI.Areas;

with Chaos.Xi_UI.Images;

package body Chaos.Xi_UI is

   type Root_Xi_UI is
     new Chaos.UI.Root_Chaos_UI with
      record
         Model  : Chaos.Xi_UI.Models.Chaos_Xi_Model;
         Window : Xi.Render_Window.Xi_Render_Window;
      end record;

   overriding procedure Start
     (UI : in out Root_Xi_UI);

   overriding procedure Stop
     (UI : in out Root_Xi_UI);

   overriding procedure Display_Text
     (UI   : in out Root_Xi_UI;
      Text : String);

   overriding function Create_Image_Container
     (UI : Root_Xi_UI)
      return Chaos.Images.Chaos_Image_Container;

   ------------
   -- Create --
   ------------

   function Create return Chaos.UI.Chaos_UI is
      Result : Root_Xi_UI;
   begin
      Xi.Main.Init;
      Result.Initialize;
      Result.Window :=
        Xi.Main.Current_Renderer.Create_Top_Level_Window;
      Result.Window.Set_Wireframe (False);
      return new Root_Xi_UI'(Result);
   end Create;

   ----------------------------
   -- Create_Image_Container --
   ----------------------------

   overriding function Create_Image_Container
     (UI : Root_Xi_UI)
      return Chaos.Images.Chaos_Image_Container
   is
      pragma Unreferenced (UI);
   begin
      return new Chaos.Xi_UI.Images.Xi_Image_Container_Record;
   end Create_Image_Container;

   ------------------
   -- Display_Text --
   ------------------

   overriding procedure Display_Text
     (UI   : in out Root_Xi_UI;
      Text : String)
   is
      pragma Unreferenced (UI);
   begin
      Ada.Text_IO.Put_Line (Text);
   end Display_Text;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (UI : in out Root_Xi_UI)
   is
   begin
      UI.Model :=
        Chaos.Xi_UI.Areas.Area_Model
          (Chaos.Game.Current_Game.Area);
      UI.Model.Scene.Active_Camera.Set_Viewport
        (UI.Window.Full_Viewport);
      UI.Window.Set_Scene (UI.Model.Scene);

      Chaos.Game.Current_Game.Start;

--        UI.Camera := Result.Area_Scene.Active_Camera;
--        UI.Camera.Set_Position (0.0, 0.0, 500.0);
--        UI.Camera.Look_At (0.0, 0.0, 0.0);
--        UI.Camera.Perspective (45.0, 100.0, 1000.0);

      if False then
         GL.Enable_Debug;
      end if;

      Xi.Main.Main_Loop;
   end Start;

   ----------
   -- Stop --
   ----------

   overriding procedure Stop
     (UI : in out Root_Xi_UI)
   is
      pragma Unreferenced (UI);
   begin
      Xi.Main.Leave_Main_Loop;
   end Stop;

end Chaos.Xi_UI;
