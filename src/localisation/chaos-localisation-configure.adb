with Ada.Directories;

with Tropos.Reader;

with Chaos.Paths;

package body Chaos.Localisation.Configure is

   procedure Read_Local_Text_File
     (Path : String);

   ---------------------
   -- Read_Local_Text --
   ---------------------

   procedure Read_Local_Text
     (Language : String)
   is
      Path : constant String :=
               Chaos.Paths.Config_File
                 ("localisation/" & Language);

      Ordinary_File : Ada.Directories.File_Kind renames
        Ada.Directories.Ordinary_File;

      procedure Process
        (Directory_Entry : Ada.Directories.Directory_Entry_Type);

      -------------
      -- Process --
      -------------

      procedure Process
        (Directory_Entry : Ada.Directories.Directory_Entry_Type)
      is
      begin
         Read_Local_Text_File
           (Ada.Directories.Full_Name (Directory_Entry));
      end Process;

   begin
      Ada.Directories.Search
        (Directory => Path,
         Pattern   => "*.txt",
         Filter    => (Ordinary_File => True, others => False),
         Process   => Process'Access);
   end Read_Local_Text;

   --------------------------
   -- Read_Local_Text_File --
   --------------------------

   procedure Read_Local_Text_File
     (Path : String)
   is
      Text_Config : constant Tropos.Configuration :=
                      Tropos.Reader.Read_Config (Path);
   begin
      for Config of Text_Config loop
         Add_Text (Config.Config_Name, Config.Value);
      end loop;
   end Read_Local_Text_File;

end Chaos.Localisation.Configure;
