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
with Xi.Render_Operation;
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
with Chaos.Features;
with Chaos.Teams;

with Chaos.Animations.Actors;
with Chaos.Creatures.Colors;

package body Chaos.Xi_UI.Areas is

   Scripts_Delay      : constant Duration := 0.2;
   Animation_Delay    : constant Duration := 0.1;
   Walk_Delay         : constant Duration := 0.02;
   Inter_Square_Steps : constant := 20;

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
         Area              : Chaos.Areas.Chaos_Area;
         Actor             : Chaos.Actors.Chaos_Actor;
         Last_Frame_Update : Ada.Calendar.Time;
         Last_Walk_Update  : Ada.Calendar.Time;
         Node              : Xi.Node.Xi_Node;
         Base              : Xi.Node.Xi_Node;
         Material          : Xi.Materials.Material.Xi_Material;
         Animation         : Chaos.Xi_UI.Animations.Xi_Animation;
         Frame             : Positive;
         Walking           : Boolean;
         Destination       : Chaos.Locations.Square_Location;
         Steps             : Natural;
         Current_Step      : Natural;
      end record;

   package Actor_Node_Vectors is
     new Ada.Containers.Vectors
       (Positive, Actor_Node);

   type Area_Model_Record is
     new Chaos.Xi_UI.Models.Root_Chaos_Xi_Model with
      record
         Scene            : Xi.Scene.Xi_Scene;
         Camera           : Xi.Camera.Xi_Camera;
         Top              : Xi.Node.Xi_Node;
         Map_Top          : Xi.Node.Xi_Node;
         Actor_Top        : Xi.Node.Xi_Node;
         Base_Top         : Xi.Node.Xi_Node;
         Area             : Chaos.Areas.Chaos_Area;
         Actor            : Chaos.Actors.Chaos_Actor;
         Actors           : Actor_Node_Vectors.Vector;
         Highlight_Square : Xi.Node.Xi_Node;
         Mouse_Cursor     : Xi.Node.Xi_Node;
         Centre_X         : Xi.Xi_Float;
         Centre_Y         : Xi.Xi_Float;
         Left_Click       : Boolean := False;
         Cursor_Index     : Natural := 0;
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

   procedure On_Square_Click
     (Model  : in out Area_Model_Record'Class;
      Square : Chaos.Locations.Square_Location);

   function To_World_Position
     (Model     : Area_Model_Record'Class;
      Pixel_Loc : Chaos.Locations.Pixel_Location;
      Z         : Xi.Xi_Float := 0.0)
      return Xi.Matrices.Vector_3;

   function To_World_Position
     (Model      : Area_Model_Record'Class;
      Square_Loc : Chaos.Locations.Square_Location;
      Z          : Xi.Xi_Float := 0.0)
      return Xi.Matrices.Vector_3
   is (Model.To_World_Position (Model.Area.To_Pixels (Square_Loc), Z));

   type Area_Frame_Listener is
     new Xi.Frame_Event.Xi_Frame_Listener_Interface with
      record
         Last_Script_Execution : Ada.Calendar.Time;
         Last_Walk_Execution   : Ada.Calendar.Time;
         Model                 : Area_Model_Access;
         Mouse_X               : Xi.Xi_Float;
         Mouse_Y               : Xi.Xi_Float;
         Key                   : Character;
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
      use Ada.Calendar;
      use Xi;
      use type Chaos.Xi_UI.Animations.Xi_Animation;
      Actor_Loc : constant Chaos.Locations.Square_Location :=
                    Actor.Actor.Location;
      Base_Loc  : Xi.Matrices.Vector_3 :=
                    Model.To_World_Position (Actor_Loc, 2.0);
      World_Loc : Xi.Matrices.Vector_3;
      New_Anim  : constant Chaos.Xi_UI.Animations.Xi_Animation :=
                    Chaos.Xi_UI.Animations.Xi_Animation
                      (Chaos.Animations.Actors.Get_Animation
                         (Actor.Actor));
      Now : constant Ada.Calendar.Time :=
              Ada.Calendar.Clock;
   begin
      if Actor.Actor.Has_Path then
         if not Actor.Walking then
            Actor.Walking := True;
            Actor.Destination :=
              Chaos.Locations.First_Square (Actor.Actor.Path);
            Actor.Steps := Inter_Square_Steps;
            Actor.Current_Step := 1;
         else
            declare
               Start  : constant Chaos.Locations.Pixel_Location :=
                          Actor.Area.To_Pixels (Actor.Actor.Location);
               Finish : constant Chaos.Locations.Pixel_Location :=
                          Actor.Area.To_Pixels
                            (Actor.Actor.First_Path_Square);
            begin
               Base_Loc (1) := Base_Loc (1)
                 + Xi_Float (Finish.X - Start.X)
                 * Xi_Float (Actor.Current_Step) / Xi_Float (Actor.Steps);
               Base_Loc (2) := Base_Loc (2)
                 - Xi_Float (Finish.Y - Start.Y)
                 * Xi_Float (Actor.Current_Step) / Xi_Float (Actor.Steps);
            end;

            if Now - Actor.Last_Walk_Update >= Walk_Delay then
               Actor.Last_Walk_Update := Now;
               Actor.Current_Step := Actor.Current_Step + 1;
               if Actor.Current_Step > Actor.Steps then
                  Actor.Actor.Update
                    (Chaos.Actors.Move_Path_Square'Access);
                  if Actor.Actor.Has_Path then
                     Actor.Current_Step := 1;
                  else
                     Actor.Walking := False;
                     Chaos.Game.Current_Game.Arrive (Actor.Actor);
                  end if;
               end if;
            end if;
         end if;
      end if;

      if New_Anim /= Actor.Animation then
         Actor.Animation := New_Anim;
         Actor.Frame := 1;
      end if;

      World_Loc := Base_Loc;
      World_Loc (2) := World_Loc (2) + 20.0;
      World_Loc (3) := 5.0;
      Actor.Node.Set_Position (World_Loc);
      Actor.Base.Set_Position (Base_Loc);

      if Now - Actor.Last_Frame_Update >= Animation_Delay then
         Actor.Last_Frame_Update := Now;
         Actor.Frame := Actor.Frame + 1;
         if Actor.Frame > Actor.Animation.Frame_Count then
            Actor.Frame := 1;
         end if;
         Actor.Node.Entity.Material.Technique (1).Pass (1).Set_Texture
           (Actor.Animation.Texture
              (Identifier  => Actor.Actor.Creature.Identifier,
               Frame_Index => Actor.Frame,
               Palette     =>
                 Chaos.Creatures.Colors.Creature_Palette
                   (Actor.Actor.Creature)));
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
      Model.Actor := null; -- Chaos.Game.Current_Game.Party.Party_Member (1);
      Model.Scene := Xi.Scene.Create_Scene;
      Model.Camera := Model.Scene.Active_Camera;
      Model.Top := Model.Scene.Create_Node ("top");
      Model.Map_Top := Model.Top.Create_Child ("map-top");
      Model.Highlight_Square := Model.Top.Create_Child ("highlight-square");
      Model.Mouse_Cursor := Model.Top.Create_Child ("mouse-cursor");
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
         Animation : constant Chaos.Xi_UI.Animations.Xi_Animation :=
                       Chaos.Xi_UI.Animations.Xi_Animation
                         (Chaos.Animations.Get_Animation
                            ("CURSORS", 1));
      begin
         Entity.Set_Texture (Animation.Texture (1));
         Entity.Material.Technique (1).Pass (1).Alpha_Discard
           (Operator => Xi.Materials.Equal,
            Value    => 0.0);
         Model.Mouse_Cursor.Set_Entity (Entity);
      end;

      declare
         Entity       : constant Xi.Entity.Xi_Entity :=
                          Xi.Shapes.Square
                            (Xi.Xi_Float (Chaos.Areas.Pixels_Per_Square / 2));
         Border_Width : constant := 3;
         Texture_Data : Xi.Color.Xi_Color_2D_Array
           (1 .. Chaos.Areas.Pixels_Per_Square,
            1 .. Chaos.Areas.Pixels_Per_Square);
      begin
         for X in Texture_Data'Range (1) loop
            for Y in Texture_Data'Range (2) loop
               if X in 1 + Border_Width ..
                 Chaos.Areas.Pixels_Per_Square - Border_Width
                 and then Y in 1 + Border_Width ..
                   Chaos.Areas.Pixels_Per_Square - Border_Width
               then
                  Texture_Data (X, Y) := (0.0, 0.0, 0.0, 0.0);
               else
                  Texture_Data (X, Y) := (1.0, 1.0, 1.0, 1.0);
               end if;
            end loop;
         end loop;

         Entity.Set_Texture
           (Xi.Texture.Create_From_Data ("highlight-square", Texture_Data));
         Entity.Material.Technique (1).Pass (1).Alpha_Discard
           (Operator => Xi.Materials.Equal,
            Value    => 0.0);
         Entity.Material.Technique (1).Pass (1).Set_Lighting_Enabled (False);

         Model.Highlight_Square.Set_Entity (Entity);
--           Model.Highlight_Square.Rotate (45.0, 0.0, 0.0, 1.0);
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

      for Feature_Index in 1 .. Area.Feature_Count loop
         declare
            Feature : constant Chaos.Features.Chaos_Feature :=
                        Area.Feature (Feature_Index);
         begin
            for Polygon_Index in 1 .. Feature.Polygon_Count loop
               declare
                  Node : constant Xi.Node.Xi_Node :=
                           Model.Map_Top.Create_Child
                             ("feature" & Integer'Image (-Feature_Index)
                              & Integer'Image (-Polygon_Index));
                  Entity : Xi.Entity.Xi_Entity;
                  Boundary : constant Chaos.Features.Feature_Polygon :=
                               Feature.Polygon (Polygon_Index);
               begin
                  Xi.Entity.Xi_New (Entity);
                  Entity.Begin_Operation (Xi.Render_Operation.Triangle_Fan);
                  for Loc of Boundary loop
                     Entity.Normal (0.0, 0.0, 1.0);
--                       Entity.Color ((1.0, 0.0, 0.0, 1.0));
                     Entity.Vertex (Model.To_World_Position (Loc, 1.0));
                  end loop;
                  Entity.End_Operation;
                  Entity.Set_Material (Xi.Assets.Material ("Xi.Blue"));
                  Node.Set_Entity (Entity);
               end;
            end loop;

            declare
               Boundary : constant Chaos.Features.Feature_Polygon :=
                            Feature.Sensitive_Area;
               Node     : constant Xi.Node.Xi_Node :=
                            Model.Map_Top.Create_Child
                              ("sensitive" & Integer'Image (-Feature_Index));
               Entity   : Xi.Entity.Xi_Entity;
            begin
               Xi.Entity.Xi_New (Entity);
               Entity.Begin_Operation (Xi.Render_Operation.Triangle_Fan);
               for Loc of Boundary loop
                  Entity.Normal (0.0, 0.0, 1.0);
                  Entity.Vertex (Model.To_World_Position (Loc, 1.0));
               end loop;
               Entity.End_Operation;
               Entity.Set_Material (Xi.Assets.Material ("Xi.Purple"));
               Node.Set_Entity (Entity);
            end;
         end;
      end loop;

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
                         Last_Walk_Execution   => Ada.Calendar.Clock,
                         Model                 => Area_Model_Access (Result),
                         Mouse_X               => 0.0,
                         Mouse_Y               => 0.0,
                         Key                   => Character'Val (0));
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
                 (Area         => Model.Area,
                  Actor        => Actor,
                  Last_Frame_Update => Ada.Calendar.Clock,
                  Last_Walk_Update => Ada.Calendar.Clock,
                  Node         =>
                    Model.Actor_Top.Create_Child (Actor.Identifier),
                  Base         => Model.Base_Top.Create_Child
                    (Actor.Identifier & "-base"),
                  Material     => Xi.Materials.Material.Xi_New_With_Defaults,
                  Animation    =>
                    Chaos.Xi_UI.Animations.Xi_Animation
                      (Chaos.Animations.Actors.Get_Animation
                           (Actor)),
                  Frame        => 1,
                  Walking      => False,
                  Destination  => Actor.Location,
                  Steps        => 0,
                  Current_Step => 0);
      Pass   : constant Xi.Materials.Pass.Xi_Material_Pass :=
                 Result.Material.Technique (1).Pass (1);
   begin
      Pass.Set_Texture
        (Result.Animation.Texture
           (Identifier  => Actor.Creature.Identifier,
            Frame_Index => 1,
            Palette     =>
              Chaos.Creatures.Colors.Creature_Palette
                (Actor.Creature)));
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
--           Result.Base.Rotate (45.0, 0.0, 0.0, 1.0);

         if Base_Material (Attitude) = null then
            Base_Material (Attitude) :=
              Create_Base_Material (Attitude);
         end if;
         Result.Base.Entity.Set_Material (Base_Material (Attitude));
      end;

      Result.Base.Set_Visible (False);
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
                    when Party    => (0.2, 0.5, 0.2, 1.0),
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

      Model : constant Area_Model_Access := Listener.Model;
      Area  : constant Chaos.Areas.Chaos_Area := Model.Area;

      Got_Mouse_Square : Boolean := False;
      Mouse_X, Mouse_Y : Xi_Float;
      World_X, World_Y : Xi_Float;
      Pixel_Loc        : Chaos.Locations.Pixel_Location;
      Mouse_Square     : Chaos.Locations.Square_Location;

      function Check_Number_Pressed (Number : Natural) return Boolean;

      function Check_Key_Pressed (Ch : Character) return Boolean;

      procedure Check_Mouse_Square;

      -----------------------
      -- Check_Key_Pressed --
      -----------------------

      function Check_Key_Pressed (Ch : Character) return Boolean is
      begin
         if Xi.Keyboard.Key_Down (Xi.Keyboard.Character_Key (Ch)) then
            if Listener.Key = Ch then
               return False;
            else
               Listener.Key := Ch;
               return True;
            end if;
         else
            if Listener.Key = Ch then
               Listener.Key := Character'Val (0);
            end if;
            return False;
         end if;
      end Check_Key_Pressed;

      ------------------------
      -- Check_Mouse_Square --
      ------------------------

      procedure Check_Mouse_Square is
         use type Chaos.Actors.Chaos_Actor;
      begin
         if not Got_Mouse_Square then

            World_X :=
              (Mouse_X - Event.Render_Target.Width / 2.0)
              + Listener.Model.Centre_X;
            World_Y :=
              (Mouse_Y - Event.Render_Target.Height / 2.0)
              + Listener.Model.Centre_Y;

            Pixel_Loc :=
              (Integer (World_X) + Area.Pixels_Across / 2,
               Area.Pixels_Down - Integer (World_Y) - Area.Pixels_Down / 2);

            if Pixel_Loc.X in 1 .. Area.Pixels_Across
              and then Pixel_Loc.Y in 1 .. Area.Pixels_Down
            then
               Mouse_Square := Area.To_Square (Pixel_Loc);
            else
               Mouse_Square := (Area.Squares_Across / 2,
                                Area.Squares_Down / 2);
            end if;
            Got_Mouse_Square := True;
         end if;
      end Check_Mouse_Square;

      --------------------------
      -- Check_Number_Pressed --
      --------------------------

      function Check_Number_Pressed (Number : Natural) return Boolean is
      begin
         return Check_Key_Pressed (Character'Val (Number + 48));
      end Check_Number_Pressed;

   begin

      Mouse_X := Xi.Mouse.Current_Mouse.State.X;
      Mouse_Y := Xi.Mouse.Current_Mouse.State.Y;

      if Now - Listener.Last_Script_Execution > Scripts_Delay then
         Chaos.Expressions.Execute
           (Listener.Model.Area.Script);
         Listener.Last_Script_Execution := Now;

      end if;

      for I in 1 .. Listener.Model.Actors.Last_Index loop
         declare
            Actor : Actor_Node := Listener.Model.Actors (I);
         begin
            Listener.Model.Animate (Actor);
            Listener.Model.Actors (I) := Actor;
         end;
      end loop;

      if Xi.Keyboard.Key_Down (Xi.Keyboard.Key_Esc) then
         Xi.Main.Leave_Main_Loop;
      end if;

      if Mouse_X /= Listener.Mouse_X
        or else Mouse_Y /= Listener.Mouse_Y
      then
         Check_Mouse_Square;

         declare
            Pixel  : constant Chaos.Locations.Pixel_Location :=
                       Listener.Model.Area.To_Pixels
                         (Mouse_Square);
         begin

            Listener.Model.Highlight_Square.Set_Position
              (Xi_Float (Pixel.X - Listener.Model.Area.Pixels_Across / 2),
               Xi_Float (Listener.Model.Area.Pixels_Down / 2 - Pixel.Y),
               1.0);

            Listener.Model.Mouse_Cursor.Set_Position
              (World_X + 16.0, World_Y - 16.0, 4.0);
            Model.Highlight_Square.Set_Visible
              (Area.Passable (Mouse_Square));

            declare
               use type Chaos.Actors.Chaos_Actor;
               Square_Cursor_Index : constant Natural :=
                                       (if Area.Has_Feature (Mouse_Square)
                                        then Area.Feature (Mouse_Square)
                                        .Cursor_Index
                                        else 0);
               New_Cursor_Index : constant Positive :=
                                    (if not Area.Passable (Mouse_Square)
                                     then 8
                                     elsif Area.Has_Actor (Mouse_Square)
                                     then 19
                                     elsif Square_Cursor_Index > 0
                                     then Square_Cursor_Index
                                     else 1);
            begin
               if New_Cursor_Index /= Listener.Model.Cursor_Index then
                  declare
                     Texture  : constant Xi.Texture.Xi_Texture :=
                                  Chaos.Xi_UI.Animations.Xi_Animation
                                    (Chaos.Animations.Get_Animation
                                       ("CURSORS", New_Cursor_Index))
                       .Texture (1);

                     Material : constant Xi.Materials.Material.Xi_Material :=
                                  Listener.Model.Mouse_Cursor.Entity.Material;
                  begin
                     Material.Technique (1).Pass (1).Set_Texture (Texture);
                     Listener.Model.Cursor_Index := New_Cursor_Index;
                  end;
               end if;
            end;

            Listener.Mouse_X := Mouse_X;
            Listener.Mouse_Y := Mouse_Y;
         end;

      end if;

      if Mouse_X < 50.0 then
         Listener.Model.Centre_X :=
           Listener.Model.Centre_X - 5.0;
      elsif Mouse_X > Event.Render_Target.Width - 50.0 then
         Listener.Model.Centre_X :=
           Listener.Model.Centre_X + 5.0;
      end if;

      if Mouse_Y < 50.0 then
         Listener.Model.Centre_Y :=
           Listener.Model.Centre_Y - 5.0;
      elsif Mouse_Y > Event.Render_Target.Height - 50.0 then
         Listener.Model.Centre_Y :=
           Listener.Model.Centre_Y + 5.0;
      end if;

      Listener.Model.Camera.Set_Position
        (Listener.Model.Centre_X, Listener.Model.Centre_Y, 1000.0);
      Listener.Model.Camera.Look_At
        (Listener.Model.Centre_X, Listener.Model.Centre_Y, 0.0);

      declare
         use Xi.Mouse;
      begin
         if Current_Mouse.State.Button (Left) = Down then
            Listener.Model.Left_Click := True;
         elsif Listener.Model.Left_Click then
            Listener.Model.Left_Click := False;
            Check_Mouse_Square;
            Listener.Model.On_Square_Click
              (Mouse_Square);
         end if;
      end;

      for I in 1 .. 5 loop
         if Check_Number_Pressed (I) then
            Chaos.Game.Current_Game.Select_Option (I);
            exit;
         end if;
      end loop;

   end Frame_Started;

   ---------------------
   -- On_Square_Click --
   ---------------------

   procedure On_Square_Click
     (Model  : in out Area_Model_Record'Class;
      Square : Chaos.Locations.Square_Location)
   is
      use type Chaos.Actors.Chaos_Actor;
      Area : constant Chaos.Areas.Chaos_Area := Model.Area;
      Actor : constant Chaos.Actors.Chaos_Actor :=
                (if Area.Has_Actor (Square)
                 then Area.Actor (Square)
                 else null);
      Pixel_Loc : constant Chaos.Locations.Pixel_Location :=
                    Model.Area.To_Pixels (Square);
      Square_2  : constant Chaos.Locations.Square_Location :=
                    Model.Area.To_Square (Pixel_Loc);
   begin
      Chaos.Logging.Log
        ("XI-AREA", Pixel_Loc.X'Img & Pixel_Loc.Y'Img
         & Square.X'Img & Square.Y'Img
         & Square_2.X'Img & Square_2.Y'Img);
      if Actor = null then
         if Model.Actor /= null
           and then Area.Passable (Square)
         then
            Chaos.Game.Current_Game.Walk_To
              (Model.Actor, Square);
         end if;
      elsif Chaos.Game.Current_Game.Party.Is_Party_Member (Actor) then
         Model.Actor := Actor;
      elsif Model.Actor /= null then
         Chaos.Game.Current_Game.Interact
           (Actor       => Model.Actor,
            Target      => Actor,
            Interaction => Chaos.Game.Default);
      end if;
   end On_Square_Click;

   -----------------------
   -- To_World_Position --
   -----------------------

   function To_World_Position
     (Model     : Area_Model_Record'Class;
      Pixel_Loc : Chaos.Locations.Pixel_Location;
      Z         : Xi.Xi_Float := 0.0)
      return Xi.Matrices.Vector_3
   is
      use Xi;
      X : constant Xi_Float :=
            Xi_Float (Pixel_Loc.X
                      - Model.Area.Pixels_Across / 2);
      Y : constant Xi_Float :=
            Xi_Float (Model.Area.Pixels_Down / 2
                      - Pixel_Loc.Y);
   begin
      return (X, Y, Z);
   end To_World_Position;

end Chaos.Xi_UI.Areas;
