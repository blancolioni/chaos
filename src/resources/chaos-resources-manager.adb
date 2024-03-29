with Ada.Directories;
with Ada.Strings.Fixed;

with WL.String_Maps;

--  with Chaos.Configuration;

with Chaos.Resources.Area;
with Chaos.Resources.Bam;
with Chaos.Resources.Bcs;
with Chaos.Resources.Biff;
with Chaos.Resources.Bmp;
with Chaos.Resources.Cre;
with Chaos.Resources.Dlg;
with Chaos.Resources.Ids;
with Chaos.Resources.Itm;
with Chaos.Resources.Key;
with Chaos.Resources.Tables;
with Chaos.Resources.Tis;
with Chaos.Resources.Wed;

with Chaos.Infinity_Engine;
with Chaos.Paths;

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
         when Bam_Resource =>
            return new Chaos.Resources.Bam.Bam_Resource;
         when Bmp_Resource =>
            return new Chaos.Resources.Bmp.Bmp_Resource;
         when Creature_Resource =>
            return new Chaos.Resources.Cre.Cre_Resource;
         when Dialog_Resource =>
            return new Chaos.Resources.Dlg.Dlg_Resource;
         when Identifier_Resource =>
            return new Chaos.Resources.Ids.Ids_Resource;
         when Item_Resource =>
            return new Chaos.Resources.Itm.Item_Resource;
         when Script_Resource =>
            return new Chaos.Resources.Bcs.Bcs_Resource;
         when Table_Resource =>
            return new Chaos.Resources.Tables.Table_Resource;
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
      Base_Path : constant String := Chaos.Infinity_Engine.Infinity_Path;
      File_Name : constant String := To_String (Reference) & "."
                    & Extension (Res_Type);
      Override_Path : constant String :=
                        Base_Path & "/override/" & File_Name;
      Local_Override : constant String :=
                         Chaos.Paths.Config_File
                           ("infinity/override/" & File_Name);

      Resource       : Resource_Access;
   begin
      if not Resource_Map.Contains (Key) then
         Resource := Create_Resource (Res_Type);

         if Ada.Directories.Exists (Local_Override) then
            Resource.Open (Reference, Local_Override);
         elsif Ada.Directories.Exists (Override_Path) then
            Resource.Open (Reference, Override_Path);
         else
            if not Got_Keys then
               Keys.Open ("CHITIN  ", Base_Path & "chitin.key");
               Keys.Load;
               Keys.Close;
               Got_Keys := True;
            end if;

            declare
               Locator : Word_32;
               Path    : constant String :=
                           Keys.Get_Resource_Location
                             (Reference, Res_Type, Locator);
            begin

               if Path = "" then
                  return null;
               end if;

               if not Biff_Map.Contains (Path) then
                  declare
                     use Ada.Strings.Fixed;
                     Reference : constant String :=
                                   Path (Index (Path, "/") + 1 .. Path'Last);
                     Biff      : constant Biff_Resource_Access :=
                                   new Chaos.Resources.Biff.Biff_Resource;
                  begin
                     Biff.Open (To_Reference (Reference), Base_Path & Path);
                     Biff.Load;
                     Biff_Map.Insert (Path, Biff);
                  end;
               end if;

               Biff_Map.Element (Path).Open_Resource
                 (Reference, Resource.all, Locator);

               Resource_Map.Insert (Key, Resource);

            end;
         end if;

         Resource.Load;

      else
         Resource := Resource_Map.Element (Key);
      end if;

      return Resource;

   end Load_Resource;

   ---------------------
   -- Resource_Exists --
   ---------------------

   function Resource_Exists
     (Reference  : Resource_Reference;
      Res_Type   : Resource_Type)
      return Boolean
   is
      Key       : constant String := To_Key (Reference, Res_Type);
      Base_Path : constant String := Chaos.Infinity_Engine.Infinity_Path;
   begin
      if not Resource_Map.Contains (Key) then
         if not Got_Keys then
            Keys.Open ("CHITIN  ", Base_Path & "chitin.key");
            Keys.Load;
            Keys.Close;
            Got_Keys := True;
         end if;

         declare
            Locator  : Word_32;
            Path     : constant String :=
                         Keys.Get_Resource_Location
                           (Reference, Res_Type, Locator);
         begin
            return Path /= "";
         end;
      else
         return True;
      end if;
   end Resource_Exists;

end Chaos.Resources.Manager;
