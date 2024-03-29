with GL;

with Xi.Font;
with Xi.Main;
with Xi.Render_Window;
with Xi.Scene;

with Xtk;
with Xtk.Builder;
with Xtk.Panel;
with Xtk.Text.Buffer;
with Xtk.Text.View;

with Chaos.Areas;
with Chaos.Game;
with Chaos.Images;
with Chaos.Animations;

with Chaos.Xi_UI.Models;
with Chaos.Xi_UI.Areas;

with Chaos.Xi_UI.Animations;
with Chaos.Xi_UI.Images;

with Chaos.Paths;

package body Chaos.Xi_UI is

   type Root_Xi_UI is
     new Chaos.UI.Root_Chaos_UI with
      record
         Model     : Chaos.Xi_UI.Models.Chaos_Xi_Model;
         Window    : Xi.Render_Window.Xi_Render_Window;
         Log       : Xtk.Text.Buffer.Xtk_Text_Buffer;
         Log_View  : Xtk.Text.View.Xtk_Text_View;
         Log_Panel : Xtk.Panel.Xtk_Panel;
         Font      : Xi.Font.Xi_Font;
      end record;

   overriding procedure Start
     (UI : in out Root_Xi_UI);

   overriding procedure Stop
     (UI : in out Root_Xi_UI);

   overriding procedure Put
     (UI   : in out Root_Xi_UI;
      Text : String);

   overriding function Create_Image_Container
     (UI : Root_Xi_UI)
      return Chaos.Images.Chaos_Image_Container;

   overriding function Create_Animation
     (UI : Root_Xi_UI)
      return Chaos.Animations.Chaos_Animation;

   overriding procedure Show_Area
     (UI       : in out Root_Xi_UI;
      New_Area : Chaos.Areas.Chaos_Area);

   ------------
   -- Create --
   ------------

   function Create return Chaos.UI.Chaos_UI is
      Result : Root_Xi_UI;
   begin
      Xi.Main.Init;
      Xtk.Initialize
        (Chaos.Paths.Config_File ("ui/chaos.css"));
      Xtk.Text.Buffer.Xtk_New (Result.Log);
      Result.Initialize;
      Result.Window :=
        Xi.Main.Current_Renderer.Create_Top_Level_Window;
      Result.Window.Set_Wireframe (False);
      Result.Window.Set_Full_Screen (True);

      declare
         Builder : constant Xtk.Builder.Xtk_Builder :=
                     Xtk.Builder.Xtk_New_From_File
                       (Chaos.Paths.Config_File
                          ("ui/area.html"));
         View    : constant Xtk.Text.View.Xtk_Text_View :=
                     Xtk.Text.View.Xtk_Text_View
                       (Builder.Get ("log-text-view"));
      begin
         Result.Log := View.Text_Buffer;
         Result.Log.Set_Font
           (Xi.Font.Get_Font ("SegoeUI", 14.0));
         Builder.Get_Page.Set_Viewport (Result.Window.Full_Viewport);
         Builder.Get_Page.Show_All;
         Result.Window.Add_Top_Level (Builder.Get_Page);
      end;

      return new Root_Xi_UI'(Result);
   end Create;

   ----------------------
   -- Create_Animation --
   ----------------------

   overriding function Create_Animation
     (UI : Root_Xi_UI)
      return Chaos.Animations.Chaos_Animation
   is
      pragma Unreferenced (UI);
   begin
      return new Chaos.Xi_UI.Animations.Xi_Animation_Record;
   end Create_Animation;

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

   overriding procedure Put
     (UI   : in out Root_Xi_UI;
      Text : String)
   is
   begin
      UI.Log.Append (Text);
   end Put;

   ---------------
   -- Show_Area --
   ---------------

   overriding procedure Show_Area
     (UI       : in out Root_Xi_UI;
      New_Area : Chaos.Areas.Chaos_Area)
   is
   begin
      UI.Model :=
        Chaos.Xi_UI.Areas.Area_Model (New_Area);
   end Show_Area;

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
