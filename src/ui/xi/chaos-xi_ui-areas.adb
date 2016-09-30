with Ada.Calendar;

with WL.String_Maps;

with Xi.Assets;
with Xi.Camera;
with Xi.Color;
with Xi.Entity;
with Xi.Frame_Event;
with Xi.Keyboard;
with Xi.Main;
with Xi.Mouse;
with Xi.Node;
with Xi.Scene;
with Xi.Shapes;
with Xi.Texture;

with Chaos.Xi_UI.Images;

with Chaos.Locations;
with Chaos.Game;

with Chaos.Expressions;

with Chaos.Logging;

package body Chaos.Xi_UI.Areas is

   package Area_Model_Maps is
     new WL.String_Maps
       (Chaos.Xi_UI.Models.Chaos_Xi_Model,
        Chaos.Xi_UI.Models."=");

   Model_Cache : Area_Model_Maps.Map;

   type Area_Model_Record is
     new Chaos.Xi_UI.Models.Root_Chaos_Xi_Model with
      record
         Scene     : Xi.Scene.Xi_Scene;
         Top       : Xi.Node.Xi_Node;
         Area      : Chaos.Areas.Chaos_Area;
         Highlight : Xi.Node.Xi_Node;
         Centre_X  : Xi.Xi_Float;
         Centre_Y  : Xi.Xi_Float;
      end record;

   type Area_Model_Access is access all Area_Model_Record'Class;

   overriding function Scene
     (Model : Area_Model_Record)
      return Xi.Scene.Xi_Scene
   is (Model.Scene);

   type Area_Frame_Listener is
     new Xi.Frame_Event.Xi_Frame_Listener_Interface with
      record
         Last_Script_Execution : Ada.Calendar.Time;
         Model                 : Area_Model_Access;
      end record;

   overriding procedure Frame_Started
     (Listener : in out Area_Frame_Listener;
      Event    : Xi.Frame_Event.Xi_Frame_Event);

   ----------------
   -- Area_Model --
   ----------------

   function Area_Model
     (Area : Chaos.Areas.Chaos_Area)
      return Chaos.Xi_UI.Models.Chaos_Xi_Model
   is
      use type Xi.Entity.Xi_Entity;

      Model : Area_Model_Record;
      Images : constant Chaos.Xi_UI.Images.Xi_Image_Container :=
                 Chaos.Xi_UI.Images.Xi_Image_Container
                   (Area.Images);
   begin
      if Model_Cache.Contains (Area.Identifier) then
         return Model_Cache.Element (Area.Identifier);
      end if;

      Chaos.Logging.Log
        ("XI", "Creating model for area " & Area.Identifier
         & Natural'Image (Area.Tiles_Across)
         & " x"
         & Natural'Image (Area.Tiles_Down));

      Model.Area := Area;
      Model.Scene := Xi.Scene.Create_Scene;
      Model.Top := Model.Scene.Create_Node ("map-top");
      Model.Highlight := Model.Scene.Create_Node ("highlight");

      declare
         Entity : constant Xi.Entity.Xi_Entity :=
                    Xi.Shapes.Square
                      (Xi.Xi_Float (Chaos.Areas.Pixels_Per_Square / 2));
      begin
         Entity.Set_Material (Xi.Assets.Material ("Xi.Blue"));
         Model.Highlight.Set_Entity (Entity);
         Model.Highlight.Rotate (40.0, 0.0, 0.0, 1.0);
      end;

      for Tile_Y in 1 .. Area.Tiles_Down loop
         declare
            use Xi;
            Y : constant Xi_Float :=
                  Xi_Float
                    ((Area.Tiles_Down / 2 - Tile_Y + 1)
                     * 64 - 32);
         begin
            for Tile_X in 1 .. Area.Tiles_Down loop
               declare
                  X          : constant Xi_Float :=
                                 Xi_Float ((Tile_X - Area.Tiles_Across / 2)
                                           * 64 - 32);
                  Name       : constant String :=
                                 "tile" & Integer'Image (-Tile_X)
                               & Integer'Image (-Tile_Y);
                  Node       : constant Xi.Node.Xi_Node :=
                                 Model.Top.Create_Child (Name);
                  Square     : constant Xi.Entity.Xi_Entity :=
                                 Xi.Shapes.Square (32.0);
                  Tile_Index : constant Positive :=
                                 Model.Area.Tile_Index (Tile_X, Tile_Y);
                  Image      : constant Xi.Color.Xi_Color_2D_Array :=
                                 Images.Tile (Tile_Index);
                  Texture    : constant Xi.Texture.Xi_Texture :=
                                 Xi.Texture.Create_From_Data
                                   (Name, Image);
               begin
                  Square.Set_Texture (Texture);
                  Node.Set_Position (X, Y, 0.0);
                  Node.Set_Entity (Square);

               end;
            end loop;
         end;
      end loop;

      Chaos.Logging.Log
        ("XI",
         "created"
         & Natural'Image (Area.Tiles_Across * Area.Tiles_Down) & " tiles");

      declare
         use Xi;
         Camera : constant Xi.Camera.Xi_Camera :=
                    Model.Scene.Active_Camera;
         Start_Loc : constant Chaos.Locations.Square_Location :=
                       Chaos.Game.Current_Game.Party.Party_Member (1)
                       .Location;
         Pixel_Loc : constant Chaos.Locations.Pixel_Location :=
                       Model.Area.To_Pixels (Start_Loc);
      begin
         Chaos.Logging.Log ("XI",
                            "start location:"
                            & Start_Loc.X'Img & Start_Loc.Y'Img);
         Chaos.Logging.Log ("XI",
                            "start pixel location:"
                            & Pixel_Loc.X'Img & Pixel_Loc.Y'Img);

         declare
            Camera_X : constant Xi_Float :=
                         Xi_Float (Pixel_Loc.X - Model.Area.Pixels_Across / 2);
            Camera_Y : constant Xi_Float :=
                         Xi_Float (Model.Area.Pixels_Down / 2 - Pixel_Loc.Y);
         begin
            Model.Centre_X := Camera_X;
            Model.Centre_Y := Camera_Y;
            Camera.Set_Position (Camera_X, Camera_Y, 1000.0);
            Camera.Look_At (Camera_X, Camera_Y, 0.0);
            Model.Highlight.Set_Position (Camera_X, Camera_Y, 10.0);
         end;

         Camera.Perspective (45.0, 100.0, 2000.0);
      end;

      declare
         Result : constant Chaos.Xi_UI.Models.Chaos_Xi_Model :=
                    new Area_Model_Record'(Model);
         Listener : constant Xi.Frame_Event.Xi_Frame_Listener :=
                      new Area_Frame_Listener'
                        (Last_Script_Execution => Ada.Calendar.Clock,
                         Model                 => Area_Model_Access (Result));
      begin
         Xi.Main.Add_Frame_Listener (Listener);
         Model_Cache.Insert (Area.Identifier, Result);
         return Result;
      end;

   end Area_Model;

   -------------------
   -- Frame_Started --
   -------------------

   overriding procedure Frame_Started
     (Listener : in out Area_Frame_Listener;
      Event    : Xi.Frame_Event.Xi_Frame_Event)
   is
      use Xi;
      use Ada.Calendar;
      Now : constant Time := Clock;
   begin
      if Now - Listener.Last_Script_Execution > 0.1 then
         Chaos.Expressions.Execute
           (Listener.Model.Area.Script);
         Listener.Last_Script_Execution := Now;
      end if;

      if Xi.Keyboard.Key_Down (Xi.Keyboard.Key_Esc) then
         Xi.Main.Leave_Main_Loop;
      end if;

      declare
         X : constant Xi_Float :=
               Xi.Mouse.Current_Mouse.State.X
                 - Event.Render_Target.Width / 2.0
                 + Listener.Model.Centre_X;
         Y : constant Xi_Float :=
               Xi.Mouse.Current_Mouse.State.Y
                 - Event.Render_Target.Height / 2.0
               + Listener.Model.Centre_Y;
      begin
         Listener.Model.Highlight.Set_Position
           (Xi_Float (Integer (X) / Chaos.Areas.Pixels_Per_Square
            * Chaos.Areas.Pixels_Per_Square),
            Xi_Float (Integer (Y) / Chaos.Areas.Pixels_Per_Square
              * Chaos.Areas.Pixels_Per_Square),
            1.0);
      end;
   end Frame_Started;

end Chaos.Xi_UI.Areas;
