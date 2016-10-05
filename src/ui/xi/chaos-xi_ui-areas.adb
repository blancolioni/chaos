with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Text_IO;

with WL.String_Maps;

with Xi.Assets;
with Xi.Camera;
with Xi.Color;
with Xi.Entity;
with Xi.Float_Images;
with Xi.Frame_Event;
with Xi.Keyboard;
with Xi.Main;
with Xi.Materials.Material;
with Xi.Materials.Pass;
with Xi.Matrices;
with Xi.Mouse;
with Xi.Node;
with Xi.Scene;
with Xi.Shapes;
with Xi.Texture;
with Xi.Value;

with Chaos.Xi_UI.Animations;
with Chaos.Xi_UI.Images;

with Chaos.Locations;
with Chaos.Game;

with Chaos.Expressions;

with Chaos.Logging;

with Chaos.Actors;
with Chaos.Teams;

with Chaos.Animations.Actors;
with Chaos.Creatures.Colors;

package body Chaos.Xi_UI.Areas is

   Base_Material : array (Chaos.Teams.Chaos_Attitude) of
     Xi.Materials.Material.Xi_Material;

   function Create_Base_Material
     (Attitude : Chaos.Teams.Chaos_Attitude)
      return Xi.Materials.Material.Xi_Material;

   package Area_Model_Maps is
     new WL.String_Maps
       (Chaos.Xi_UI.Models.Chaos_Xi_Model,
        Chaos.Xi_UI.Models."=");

   Model_Cache : Area_Model_Maps.Map;

   type Actor_Node is
      record
         Actor     : Chaos.Actors.Chaos_Actor;
         Node      : Xi.Node.Xi_Node;
         Base      : Xi.Node.Xi_Node;
         Material  : Xi.Materials.Material.Xi_Material;
         Animation : Chaos.Xi_UI.Animations.Xi_Animation;
         Frame     : Positive;
      end record;

   package Actor_Node_Vectors is
     new Ada.Containers.Vectors
       (Positive, Actor_Node);

   type Area_Model_Record is
     new Chaos.Xi_UI.Models.Root_Chaos_Xi_Model with
      record
         Scene     : Xi.Scene.Xi_Scene;
         Top       : Xi.Node.Xi_Node;
         Map_Top   : Xi.Node.Xi_Node;
         Actor_Top : Xi.Node.Xi_Node;
         Base_Top  : Xi.Node.Xi_Node;
         Area      : Chaos.Areas.Chaos_Area;
         Actors    : Actor_Node_Vectors.Vector;
         Highlight : Xi.Node.Xi_Node;
         Centre_X  : Xi.Xi_Float;
         Centre_Y  : Xi.Xi_Float;
      end record;

   type Area_Model_Access is access all Area_Model_Record'Class;

   overriding function Scene
     (Model : Area_Model_Record)
      return Xi.Scene.Xi_Scene
   is (Model.Scene);

   function Create_Actor_Node
     (Model : Area_Model_Record'Class;
      Actor : Chaos.Actors.Chaos_Actor)
      return Actor_Node;

   procedure Animate
     (Model : Area_Model_Record'Class;
      Actor : in out Actor_Node);

   type Area_Frame_Listener is
     new Xi.Frame_Event.Xi_Frame_Listener_Interface with
      record
         Last_Script_Execution : Ada.Calendar.Time;
         Model                 : Area_Model_Access;
         Mouse_X               : Xi.Xi_Float;
         Mouse_Y               : Xi.Xi_Float;
      end record;

   overriding procedure Frame_Started
     (Listener : in out Area_Frame_Listener;
      Event    : Xi.Frame_Event.Xi_Frame_Event);

   -------------
   -- Animate --
   -------------

   procedure Animate
     (Model : Area_Model_Record'Class;
      Actor : in out Actor_Node)
   is
      use Xi;
      Actor_Loc : constant Chaos.Locations.Square_Location :=
                    Actor.Actor.Location;
      Pixel_Loc : constant Chaos.Locations.Pixel_Location :=
                    Model.Area.To_Pixels (Actor_Loc);
      Base_Loc  : constant Xi.Matrices.Vector_3 :=
                    (Xi_Float (Pixel_Loc.X - Model.Area.Pixels_Across / 2),
                     Xi_Float (Model.Area.Pixels_Down / 2 - Pixel_Loc.Y),
                     1.0);
      World_Loc : Xi.Matrices.Vector_3 := Base_Loc;
   begin
      World_Loc (3) := 5.0;
      Actor.Node.Set_Position (World_Loc);
      Actor.Node.Entity.Material.Technique (1).Pass (1).Set_Texture
        (Actor.Animation.Texture
           (Identifier  => Actor.Actor.Creature.Identifier,
            Frame_Index => Actor.Frame,
            Palette     =>
              Chaos.Creatures.Colors.Creature_Palette
                (Actor.Actor.Creature)));
      Actor.Base.Set_Position (Base_Loc);
      Actor.Frame := Actor.Frame + 1;
      if Actor.Frame > Actor.Animation.Frame_Count then
         Actor.Frame := 1;
      end if;
      Actor.Node.Scale
        (Xi_Float (Actor.Animation.Frame_Width (Actor.Frame)),
         Xi_Float (Actor.Animation.Frame_Height (Actor.Frame)),
         1.0);
   end Animate;

   ----------------
   -- Area_Model --
   ----------------

   function Area_Model
     (Area : Chaos.Areas.Chaos_Area)
      return Chaos.Xi_UI.Models.Chaos_Xi_Model
   is
      use type Xi.Entity.Xi_Entity;

      Model  : Area_Model_Record;
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
      Model.Top := Model.Scene.Create_Node ("top");
      Model.Map_Top := Model.Top.Create_Child ("map-top");
      Model.Highlight := Model.Top.Create_Child ("highlight");
      Model.Base_Top := Model.Top.Create_Child ("base-top");
      Model.Actor_Top := Model.Top.Create_Child ("actor-top");

      for I in 1 .. Model.Area.Actor_Count loop
         Model.Actors.Append
           (Create_Actor_Node (Model, Model.Area.Actor (I)));
      end loop;

      declare
         Entity : constant Xi.Entity.Xi_Entity :=
                    Xi.Shapes.Square
                      (Xi.Xi_Float (Chaos.Areas.Pixels_Per_Square / 2));
      begin
         Entity.Set_Material (Xi.Assets.Material ("Xi.Blue"));
         Model.Highlight.Set_Entity (Entity);
         Model.Highlight.Rotate (45.0, 0.0, 0.0, 1.0);
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
                                 Model.Map_Top.Create_Child (Name);
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
         Camera    : constant Xi.Camera.Xi_Camera :=
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
            Check : constant Chaos.Locations.Square_Location :=
                      Model.Area.To_Square (Pixel_Loc);
         begin
            Chaos.Logging.Log ("XI",
                               "check square location:"
                               & Check.X'Img & Check.Y'Img);
         end;

         declare
            Camera_X : constant Xi_Float :=
                         Xi_Float (Pixel_Loc.X
                                   - Model.Area.Pixels_Across / 2);
            Camera_Y : constant Xi_Float :=
                         Xi_Float (Model.Area.Pixels_Down / 2
                                   - Pixel_Loc.Y);
         begin
            Model.Centre_X := Camera_X;
            Model.Centre_Y := Camera_Y;
            Camera.Set_Position (Camera_X, Camera_Y, 1000.0);
            Camera.Look_At (Camera_X, Camera_Y, 0.0);
            Model.Highlight.Set_Position (Camera_X, Camera_Y, 1.0);
            Ada.Text_IO.Put_Line
              ("start: "
               & Xi.Float_Images.Image (Camera_X)
               & ", "
               & Xi.Float_Images.Image (Camera_Y));
         end;

         Camera.Perspective (45.0, 100.0, 2000.0);
      end;

      declare
         Result   : constant Chaos.Xi_UI.Models.Chaos_Xi_Model :=
                      new Area_Model_Record'(Model);
         Listener : constant Xi.Frame_Event.Xi_Frame_Listener :=
                      new Area_Frame_Listener'
                        (Last_Script_Execution => Ada.Calendar.Clock,
                         Model                 => Area_Model_Access (Result),
                         Mouse_X               => 0.0,
                         Mouse_Y               => 0.0);
      begin
         Xi.Main.Add_Frame_Listener (Listener);
         Model_Cache.Insert (Area.Identifier, Result);
         return Result;
      end;

   end Area_Model;

   -----------------------
   -- Create_Actor_Node --
   -----------------------

   function Create_Actor_Node
     (Model : Area_Model_Record'Class;
      Actor : Chaos.Actors.Chaos_Actor)
      return Actor_Node
   is
      Result : Actor_Node :=
                 (Actor     => Actor,
                  Node      => Model.Actor_Top.Create_Child (Actor.Identifier),
                  Base      => Model.Base_Top.Create_Child
                    (Actor.Identifier & "-base"),
                  Material  => Xi.Materials.Material.Xi_New_With_Defaults,
                  Animation =>
                    Chaos.Xi_UI.Animations.Xi_Animation
                      (Chaos.Animations.Actors.Get_Standing_Animation
                           (Actor)),
                  Frame     => 1);
      Pass   : constant Xi.Materials.Pass.Xi_Material_Pass :=
                 Result.Material.Technique (1).Pass (1);
   begin
      Pass.Set_Texture (Result.Animation.Texture (1));
      Pass.Alpha_Discard
        (Xi.Materials.Equal, Value => 0.0);

      declare
         use type Xi.Materials.Material.Xi_Material;
         Attitude : constant Chaos.Teams.Chaos_Attitude :=
                      (if Chaos.Game.Current_Game.Party.Is_Party_Member
                         (Actor)
                       then Chaos.Teams.Party
                       else Chaos.Teams.Neutral);
      begin
         Result.Base.Set_Entity
           (Xi.Shapes.Square (16.0));
         Result.Base.Rotate (45.0, 0.0, 0.0, 1.0);

         if Base_Material (Attitude) = null then
            Base_Material (Attitude) :=
              Create_Base_Material (Attitude);
         end if;
         Result.Base.Entity.Set_Material (Base_Material (Attitude));
      end;

      Result.Node.Set_Entity (Xi.Shapes.Square (0.5));
      Result.Node.Entity.Set_Material (Result.Material);
      Model.Animate (Result);
      return Result;
   end Create_Actor_Node;

   --------------------------
   -- Create_Base_Material --
   --------------------------

   function Create_Base_Material
     (Attitude : Chaos.Teams.Chaos_Attitude)
      return Xi.Materials.Material.Xi_Material
   is
      use all type Chaos.Teams.Chaos_Attitude;
      Color : constant Xi.Color.Xi_Color :=
                (case Attitude is
                    when Party    => (0.0, 0.8, 0.0, 1.0),
                    when Friendly => (0.0, 0.6, 0.0, 1.0),
                    when Neutral  => (0.0, 0.7, 0.7, 1.0),
                    when Hostile  => (0.8, 0.0, 0.0, 1.0));
      Material : constant Xi.Materials.Material.Xi_Material :=
                   Xi.Assets.Material ("Xi.Solid_Color").Instantiate;
   begin
      Material.Set_Parameter_Value ("color", Xi.Value.Color_Value (Color));
      return Material;
   end Create_Base_Material;

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

         for I in 1 .. Listener.Model.Actors.Last_Index loop
            declare
               Actor : Actor_Node := Listener.Model.Actors (I);
            begin
               Listener.Model.Animate (Actor);
               Listener.Model.Actors (I) := Actor;
            end;
         end loop;

      end if;

      if Xi.Keyboard.Key_Down (Xi.Keyboard.Key_Esc) then
         Xi.Main.Leave_Main_Loop;
      end if;

      if Xi.Mouse.Current_Mouse.State.X /= Listener.Mouse_X
        or else Xi.Mouse.Current_Mouse.State.Y /= Listener.Mouse_Y
      then
         Listener.Mouse_X := Xi.Mouse.Current_Mouse.State.X;
         Listener.Mouse_Y := Xi.Mouse.Current_Mouse.State.Y;
         --           declare
         --              use Xi.Matrices, Xi.Float_Arrays;
         --              X              : constant Xi_Float :=
         --                                 Listener.Mouse_X * 2.0
         --                                  / Event.Render_Target.Width - 1.0;
         --              Y              : constant Xi_Float :=
         --                                 Listener.Mouse_Y * 2.0
         --                                / Event.Render_Target.Height - 1.0;
         --              Camera         : constant Xi.Camera.Xi_Camera :=
         --                                 Listener.Model.Scene.Active_Camera;
         --              Matrix         : constant Matrix_4 :=
         --                              Camera.Inverse_Transformation_Matrix;
         --              Screen_Pos     : constant Vector_4 :=
         --                              (if True
         --                      then (Listener.Mouse_X, Listener.Mouse_Y,
         --                                 0.0, 1.0)
         --                               else (X, Y, 0.0, 1.0));
         --              World_Position : Vector_4 := Matrix * Screen_Pos;
         --           begin
         --              Ada.Text_IO.Put_Line
         --             ("mouse: " & Xi.Float_Images.Image (Listener.Mouse_X)
         --                 & ", " & Xi.Float_Images.Image (Listener.Mouse_Y)
         --                 & " -> "
         --                 & Xi.Float_Images.Image (X)
         --                 & ", "
         --                 & Xi.Float_Images.Image (Y)
         --                 & " -> "
         --                 & Xi.Float_Images.Image (World_Position (1))
         --                 & ", "
         --                 & Xi.Float_Images.Image (World_Position (2)));
         --
         --              World_Position (3) := 1.0;
         --              Listener.Model.Highlight.Set_Position
         --                (World_Position (1 .. 3));
         --           end;
         declare
            use type Chaos.Actors.Chaos_Actor;
            Area   : constant Chaos.Areas.Chaos_Area := Listener.Model.Area;
            X      : constant Xi_Float :=
                       (Listener.Mouse_X - Event.Render_Target.Width / 2.0)
                  + Listener.Model.Centre_X
                  + Xi_Float (Area.Pixels_Across / 2);
            Y      : constant Xi_Float :=
                  Xi_Float (Area.Pixels_Down) -
                  ((Listener.Mouse_Y - Event.Render_Target.Height / 2.0)
                   + Listener.Model.Centre_Y
                   + Xi_Float (Area.Pixels_Down / 2));
            Square : constant Chaos.Locations.Square_Location :=
                       Area.To_Square
                         ((Integer (X),
                          Integer (Y)));
            Pixel  : constant Chaos.Locations.Pixel_Location :=
                       Listener.Model.Area.To_Pixels (Square);
            Actor  : constant Chaos.Actors.Chaos_Actor :=
                       Area.Actor (Square);
         begin

            Listener.Model.Highlight.Set_Position
              (Xi_Float (Pixel.X - Listener.Model.Area.Pixels_Across / 2),
               Xi_Float (Listener.Model.Area.Pixels_Down / 2 - Pixel.Y),
               1.0);

            if Actor /= null then
               Actor.Log (Actor.Long_Name);
            end if;
         end;

      end if;
   end Frame_Started;

end Chaos.Xi_UI.Areas;
