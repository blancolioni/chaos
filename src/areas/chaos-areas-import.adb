with Chaos.Resources.Area;
with Chaos.Resources.Bcs;
with Chaos.Resources.Tis;
with Chaos.Resources.Wed;

with Chaos.Resources.Manager;

with Chaos.Creatures.Import;
with Chaos.Expressions.Import;

with Chaos.Areas.Db;

with Chaos.UI;

package body Chaos.Areas.Import is

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

      procedure Create (Area : in out Chaos_Area_Record'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Area : in out Chaos_Area_Record'Class) is
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
               Area.Script := Chaos.Expressions.Import.Import_Script (Script);
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

         Area.Images :=
           Chaos.UI.Current_UI.Create_Image_Container;
         Area.Images.Import_Tileset (Tis);

      end Create;

      Area : constant Chaos_Area :=
               Db.Create (Create'Access);

   begin

      for Actor_Entry of Are.Actors loop
         declare
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
                                 Natural (Actor_Entry.Current_Y))));
         begin
            pragma Unreferenced (Actor);
         end;
      end loop;

      return Area;

   end Import_Area;

end Chaos.Areas.Import;
