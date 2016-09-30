with WL.String_Maps;

with Xi.Camera;
with Xi.Color;
with Xi.Entity;
with Xi.Node;
with Xi.Scene;
with Xi.Shapes;
with Xi.Texture;

with Chaos.Xi_UI.Images;

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
         Scene : Xi.Scene.Xi_Scene;
         Area  : Chaos.Areas.Chaos_Area;
      end record;

   overriding function Scene
     (Model : Area_Model_Record)
      return Xi.Scene.Xi_Scene
   is (Model.Scene);

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

      for Tile_Y in 1 .. Area.Tiles_Down loop
         for Tile_X in 1 .. Area.Tiles_Down loop
            declare
               use Xi;
               X : constant Xi_Float :=
                     Xi_Float ((Tile_X - Area.Tiles_Across / 2) * 64 - 32);
               Y : constant Xi_Float :=
                     Xi_Float
                       ((Area.Tiles_Down / 2 - Tile_Y + 1)
                        * 64 - 32);
               Name : constant String :=
                        "tile" & Integer'Image (-Tile_X)
                      & Integer'Image (-Tile_Y);
               Node : constant Xi.Node.Xi_Node :=
                        Model.Scene.Create_Node (Name);
               Square : constant Xi.Entity.Xi_Entity :=
                          Xi.Shapes.Square (64.0);
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
      end loop;

      Chaos.Logging.Log
        ("XI",
         "created"
         & Natural'Image (Area.Tiles_Across * Area.Tiles_Down) & " tiles");

      declare
         use type Xi.Xi_Float;
         Camera : constant Xi.Camera.Xi_Camera :=
                    Model.Scene.Active_Camera;
      begin
         Camera.Set_Position (0.0, 0.0, 1500.0);
         Camera.Look_At (0.0, 0.0, 0.0);
         Camera.Perspective (45.0, 100.0, 2000.0);
      end;

      declare
         Result : constant Chaos.Xi_UI.Models.Chaos_Xi_Model :=
                    new Area_Model_Record'(Model);
      begin
         Model_Cache.Insert (Area.Identifier, Result);
         return Result;
      end;

   end Area_Model;

end Chaos.Xi_UI.Areas;
