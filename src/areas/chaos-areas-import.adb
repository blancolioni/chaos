with Chaos.Resources.Area;
with Chaos.Resources.Bcs;
with Chaos.Resources.Tis;
with Chaos.Resources.Wed;

with Chaos.Resources.Manager;

with Chaos.Creatures.Import;
with Chaos.Expressions.Import;

with Chaos.Areas.Db;

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
      pragma Unreferenced (Tis);

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
