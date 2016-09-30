with WL.String_Maps;

with Chaos.Configuration;

with Chaos.Resources.Area;
with Chaos.Resources.Bcs;
with Chaos.Resources.Biff;
with Chaos.Resources.Cre;
with Chaos.Resources.Dlg;
with Chaos.Resources.Key;
with Chaos.Resources.Tis;
with Chaos.Resources.Wed;

package body Chaos.Resources.Manager is

   Keys : Chaos.Resources.Key.Key_Resource;
   Got_Keys : Boolean := False;

   type Resource_Access is access all Chaos_Resource'Class;

   function Create_Resource
     (Res_Type : Resource_Type)
      return Resource_Access;

   package Resource_Maps is
     new WL.String_Maps (Resource_Access);

   Resource_Map : Resource_Maps.Map;

   function To_Key
     (Reference : Resource_Reference;
      Res_Type  : Resource_Type)
      return String
   is (String (Reference) & Resource_Type'Image (Res_Type));

   type Biff_Resource_Access is
     access all Chaos.Resources.Biff.Biff_Resource'Class;

   package Biff_Maps is
     new WL.String_Maps (Biff_Resource_Access);

   Biff_Map : Biff_Maps.Map;

   ---------------------
   -- Create_Resource --
   ---------------------

   function Create_Resource
     (Res_Type : Resource_Type)
      return Resource_Access
   is
   begin
      case Res_Type is
         when Area_Resource =>
            return new Chaos.Resources.Area.Area_Resource;
         when Creature_Resource =>
            return new Chaos.Resources.Cre.Cre_Resource;
         when Dialog_Resource =>
            return new Chaos.Resources.Dlg.Dlg_Resource;
         when Script_Resource =>
            return new Chaos.Resources.Bcs.Bcs_Resource;
         when Tileset_Resource =>
            return new Chaos.Resources.Tis.Tis_Resource;
         when Wed_Resource =>
            return new Chaos.Resources.Wed.Wed_Resource;
         when others =>
            return null;
      end case;
   end Create_Resource;

   -------------------
   -- Load_Resource --
   -------------------

   function Load_Resource
     (Reference  : Resource_Reference;
      Res_Type   : Resource_Type)
      return access constant Chaos_Resource'Class
   is
      Key : constant String := To_Key (Reference, Res_Type);
      Base_Path : constant String :=
                    Chaos.Configuration.Infinity_Path;
   begin
      if not Resource_Map.Contains (Key) then
         if not Got_Keys then
            Keys.Open (Base_Path & "chitin.key");
            Keys.Load;
            Keys.Close;
            Got_Keys := True;
         end if;

         declare
            Locator : Word_32;
            Path    : constant String :=
                        Keys.Get_Resource_Location
                          (Reference, Res_Type, Locator);
            Resource : Resource_Access;
         begin

            if Path = "" then
               return null;
            end if;

            Resource := Create_Resource (Res_Type);
            if not Biff_Map.Contains (Path) then
               declare
                  Biff : constant Biff_Resource_Access :=
                           new Chaos.Resources.Biff.Biff_Resource;
               begin
                  Biff.Open (Base_Path & Path);
                  Biff.Load;
                  Biff_Map.Insert (Path, Biff);
               end;
            end if;

            Biff_Map.Element (Path).Open_Resource (Resource.all, Locator);

            Resource.Load;

            Resource_Map.Insert (Key, Resource);

         end;
      end if;

      return Resource_Map.Element (Key);

   end Load_Resource;

end Chaos.Resources.Manager;
