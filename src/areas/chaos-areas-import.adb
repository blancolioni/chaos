with WL.Binary_IO;

with Chaos.Resources.Area;
with Chaos.Resources.Bcs;
with Chaos.Resources.Bmp;
with Chaos.Resources.Tis;
with Chaos.Resources.Wed;

with Chaos.Resources.Manager;

with Chaos.Creatures.Import;
with Chaos.Expressions.Import;
with Chaos.Features.Import;

with Chaos.Areas.Db;

with Chaos.UI;

package body Chaos.Areas.Import is

   type Search_Map_Index is
     (Obstacle, Sand, Wood_1, Wood_2, Stone, Grass_1,
      Water_Passable_1, Stone_Hard,
      Transparent_Obstacle, Wood_3,
      Impassable_Wall, Water_Passable_2, Water_Impassable,
      Roof, Worldmap_Exit, Grass_2);

   Transparent : constant array (Search_Map_Index) of Boolean :=
                   (Obstacle => False, Impassable_Wall => False,
                    others   => True);

   Passable : constant array (Search_Map_Index) of Boolean :=
                (Obstacle             => False,
                 Sand                 => True,
                 Wood_1               => True,
                 Wood_2               => True,
                 Stone                => True,
                 Grass_1              => True,
                 Water_Passable_1     => True,
                 Stone_Hard           => True,
                 Transparent_Obstacle => False,
                 Wood_3               => True,
                 Impassable_Wall      => False,
                 Water_Passable_2     => True,
                 Water_Impassable     => False,
                 Roof                 => False,
                 Worldmap_Exit        => True,
                 Grass_2              => True);

   ---------------------
   -- Import_Area --
   ---------------------

   function Import_Area
     (Name : String)
      return Chaos_Area
   is
      Are : Chaos.Resources.Area.Area_Resource'Class renames
              Chaos.Resources.Area.Area_Resource'Class
                (Chaos.Resources.Manager.Load_Resource
                   (Reference => Chaos.Resources.To_Reference (Name),
                    Res_Type  => Chaos.Resources.Area_Resource).all);

      Wed : Chaos.Resources.Wed.Wed_Resource'Class renames
              Chaos.Resources.Wed.Wed_Resource'Class
                (Chaos.Resources.Manager.Load_Resource
                   (Reference => Are.Wed_Resource,
                    Res_Type  => Chaos.Resources.Wed_Resource).all);

      Tis : Chaos.Resources.Tis.Tis_Resource'Class renames
              Chaos.Resources.Tis.Tis_Resource'Class
                (Chaos.Resources.Manager.Load_Resource
                   (Reference => Wed.Overlays.Element (1).Tileset_Name,
                    Res_Type  => Chaos.Resources.Tileset_Resource).all);

      SR : Chaos.Resources.Bmp.Bmp_Resource'Class renames
             Chaos.Resources.Bmp.Bmp_Resource'Class
               (Chaos.Resources.Manager.Load_Resource
                  (Reference => Chaos.Resources.To_Reference (Name & "SR"),
                   Res_Type  => Chaos.Resources.Bmp_Resource).all);

      procedure Create (Area : in out Chaos_Area_Record'Class) is null;

      procedure Configure (Area : in out Chaos_Area_Record'Class);

      procedure Configure (Area : in out Chaos_Area_Record'Class) is
      begin
         Area.Create
           (Identity     => Name,
            Pixel_Width  => Natural (Wed.Overlays.First_Element.Width) * 64,
            Pixel_Height => Natural (Wed.Overlays.First_Element.Height) * 64);
         if Chaos.Resources.Has_Resource (Are.Area_Script) then
            declare
               Script : Chaos.Resources.Bcs.Bcs_Resource'Class renames
                          Chaos.Resources.Bcs.Bcs_Resource'Class
                            (Chaos.Resources.Manager.Load_Resource
                               (Reference => Are.Area_Script,
                                Res_Type  =>
                                  Chaos.Resources.Script_Resource).all);
            begin
               Chaos.Expressions.Import.Import_Script (Script);
               Area.Script := Chaos.Expressions.Store.Pop;
            end;
         end if;

         declare
            Tile_Lookup : Resources.Wed.Tile_Index_Vectors.Vector renames
                            Wed.Overlays.Element (1).Tile_Indices;
            Tile_Map : Resources.Wed.Tile_Map_Entry_Vectors.Vector renames
                         Wed.Overlays.Element (1).Tile_Map;
         begin
            for Y in 1 .. Area.Tiles_Down loop
               for X in 1 .. Area.Tiles_Across loop
                  declare
                     Map_Index : constant Positive :=
                                   X + (Y - 1) * Area.Tiles_Across;
                     Lookup_Index : constant Natural :=
                                      Natural
                                        (Tile_Map.Element
                                           (Map_Index).Start_Index);
                     Tile_Index   : constant Natural :=
                                      Natural
                                        (Tile_Lookup.Element
                                           (Lookup_Index + 1));
                  begin
                     Area.Tiles.Append ((Tile_Index => Tile_Index + 1));
                  end;
               end loop;
            end loop;
         end;

         for Square_Index in 1 .. Area.Squares.Last_Index loop
            declare
               Square_Loc : constant Chaos.Locations.Square_Location :=
                              Area.To_Square_Location (Square_Index);
               Pixel_Loc  : constant Chaos.Locations.Pixel_Location :=
                              Area.To_Pixels (Square_Loc);
            begin
               if Pixel_Loc.X in 0 .. Area.Pixel_Width - 1
                 and then Pixel_Loc.Y in 0 .. Area.Pixel_Height - 1
               then
                  declare
                     Bmp_X : constant Natural :=
                               Pixel_Loc.X * SR.Width / Area.Pixel_Width;
                     Bmp_Y : constant Natural :=
                               SR.Height -
                                 Pixel_Loc.Y * SR.Height / Area.Pixel_Height
                                   - 1;
                     Index : constant Natural :=
                               SR.Color_Index (Bmp_X, Bmp_Y);
                     Feature : constant Search_Map_Index :=
                                 Search_Map_Index'Val (Index);
                     Square  : Square_Type renames Area.Squares (Square_Index);
                  begin
                     Square.Transparent := Transparent (Feature);
                     Square.Passable := Passable (Feature);
                  end;
               end if;
            end;
         end loop;

         Area.Images :=
           Chaos.UI.Current_UI.Create_Image_Container;
         Area.Images.Import_Tileset (Tis);

         for I in 1 .. Wed.Doors.Last_Index loop
            declare
               use Chaos.Resources;
               Ref : constant Resource_Reference :=
                       Wed.Doors.Element (I).Name;
            begin
               for J in 1 .. Are.Doors.Last_Index loop
                  if Are.Doors.Element (J).Resource_Name = Ref then
                     Area.Features.Append
                       (Chaos.Features.Import.Import_Door
                          (Are, Wed, J, I));
                     exit;
                  end if;
               end loop;
            end;
         end loop;

         for Region_Index in 1 .. Are.Regions.Last_Index loop
            declare
               Feature      : constant Chaos.Features.Chaos_Feature :=
                                Chaos.Features.Import.Import_Region
                                  (Are, Region_Index);
               Box          : constant Chaos.Locations.Pixel_Rectangle :=
                                Feature.Bounding_Box;
               Pixel_Centre : constant Chaos.Locations.Pixel_Location :=
                                ((Box.X1 + Box.X2) / 2,
                                 (Box.Y1 + Box.Y2) / 2);
               Square_Loc   : constant Chaos.Locations.Square_Location :=
                                Area.To_Square (Pixel_Centre);
               Square_Index : constant Positive :=
                                Area.To_Square_Index (Square_Loc);
            begin
               Area.Squares (Square_Index).Feature := Feature;
            end;
         end loop;

         for Entrance of Are.Entrances loop
            declare
               Square_Loc : constant Chaos.Locations.Square_Location :=
                              Area.To_Square
                                ((Integer (Entrance.X), Integer (Entrance.Y)));
            begin
               Area.Entrances.Append
                 ((Entrance.Name, Square_Loc));
            end;
         end loop;

      end Configure;

      Area : constant Chaos_Area :=
               Db.Create (Create'Access);

   begin

      Area.Save_Object;

      Db.Update (Area.Reference, Configure'Access);

      for Actor_Entry of Are.Actors loop
         declare
            use type WL.Binary_IO.Word_16;
            Creature : constant Chaos.Creatures.Chaos_Creature :=
                         Chaos.Creatures.Import.Import_Creature
                           (String (Actor_Entry.CRE_File));
            Actor    : constant Chaos.Actors.Chaos_Actor :=
                         Chaos.Actors.Create_Actor
                           (From_Creature => Creature,
                            Area          => Area,
                            Location      =>
                              Area.To_Square
                                ((Natural (Actor_Entry.Current_X),
                                 Natural (Actor_Entry.Current_Y))),
                            Orientation   =>
                              Chaos.Locations.Orientation'Val
                                (Actor_Entry.Orientation / 2));
         begin
            pragma Unreferenced (Actor);
         end;
      end loop;

      return Area;

   end Import_Area;

end Chaos.Areas.Import;
