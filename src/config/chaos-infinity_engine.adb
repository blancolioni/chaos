with Ada.Containers.Ordered_Maps;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Tropos.Reader;
with Tropos.Writer;
with Chaos.Paths;

with Chaos.Resources.Manager;
with Chaos.Resources.Tables;

package body Chaos.Infinity_Engine is

   Infinity_Config : Tropos.Configuration;

   type Avatar_Base_Names is
     array (1 .. 4) of Ada.Strings.Unbounded.Unbounded_String;

   type Avatar_Entry is
      record
         Base_Names  : Avatar_Base_Names;
         Avatar_Type : Natural;
         Space       : Natural;
         Palette     : Natural;
         Size        : Character;
      end record;

   package Avatar_Entry_Maps is
     new Ada.Containers.Ordered_Maps (Natural, Avatar_Entry);

   Avatar_Entries : Avatar_Entry_Maps.Map;
   Got_Avatar_Entries : Boolean := False;

   procedure Load_Avatar_Entries;

   --------------------
   -- Animation_Code --
   --------------------

   function Animation_Code
     (Animation_Id : Positive)
      return String
   is
   begin
      if not Got_Avatar_Entries then
         Load_Avatar_Entries;
      end if;

      declare
         use Avatar_Entry_Maps;
         Position : Cursor := Avatar_Entries.Find (Animation_Id);
      begin
         if not Has_Element (Position) then
            Position := Avatar_Entries.Find (16#EE00#);
         end if;

         return Ada.Strings.Unbounded.To_String
           (Element (Position).Base_Names (1));
      end;
   end Animation_Code;

   -------------------
   -- Infinity_Path --
   -------------------

   function Infinity_Path return String is
   begin
      return Infinity_Config.Get ("path");
   end Infinity_Path;

   -------------------------
   -- Load_Avatar_Entries --
   -------------------------

   procedure Load_Avatar_Entries is
      Table : Chaos.Resources.Tables.Table_Resource'Class renames
              Chaos.Resources.Tables.Table_Resource'Class
                (Chaos.Resources.Manager.Load_Resource
                   (Reference => Resources.To_Reference ("avatars"),
                    Res_Type  => Chaos.Resources.Table_Resource).all);
   begin
      for Row in 1 .. Table.Row_Count loop
         declare
            use Ada.Strings.Unbounded;
            Avatar  : Avatar_Entry;
            Id_Text : constant String := Table.Row_Heading (Row);
            Id      : constant Natural :=
                        (if Id_Text'Length > 2
                         and then Id_Text (1 .. 2) = "0x"
                         then Natural'Value
                           ("16#" & Id_Text (3 .. Id_Text'Last) & "#")
                         else Natural'Value (Id_Text));
         begin
            for I in 1 .. 4 loop
               Avatar.Base_Names (I) :=
                 To_Unbounded_String (String'(Table.Get (Row, I)));
            end loop;
            Avatar.Avatar_Type := Table.Get (Row, 5);
            Avatar.Space := Table.Get (Row, 6);
            Avatar.Palette := 0;
            declare
               S : constant String :=
                     Table.Get (Row, 8);
            begin
               Avatar.Size := S (S'First);
            end;

            Avatar_Entries.Insert (Id, Avatar);
         end;
      end loop;
      Got_Avatar_Entries := True;
   end Load_Avatar_Entries;

   --------------------------
   -- Read_Infinity_Config --
   --------------------------

   procedure Read_Infinity_Config is
      Override_Path : constant String :=
                        Chaos.Paths.Config_File ("infinity-local.txt");
      Infinity_Path : constant String :=
                        Chaos.Paths.Config_File
                          ("infinity/infinity.txt");
   begin
      if Ada.Directories.Exists (Override_Path) then
         Infinity_Config :=
           Tropos.Reader.Read_Config (Override_Path);
      elsif Ada.Directories.Exists (Infinity_Path) then
         Infinity_Config :=
           Tropos.Reader.Read_Config (Infinity_Path);
         Tropos.Writer.Write_Config (Infinity_Config, Override_Path);
      else
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Cannot find infinity engine config in "
            & Chaos.Paths.Config_File ("infinity/infinity.txt"));
         raise Program_Error;
      end if;
   end Read_Infinity_Config;

end Chaos.Infinity_Engine;
