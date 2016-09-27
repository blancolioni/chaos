with Ada.Strings.Unbounded;

with Tropos.Reader;

with Chaos.Classes.Configure;
with Chaos.Powers.Configure;
with Chaos.Races.Configure;
with Chaos.Teams.Configure;

with Chaos.Paths;

package body Chaos.Configuration is

   Local_Infinity_Path : Ada.Strings.Unbounded.Unbounded_String;

   procedure Read_Infinity_Config
     (Config : Tropos.Configuration);

   -------------------
   -- Infinity_Path --
   -------------------

   function Infinity_Path return String is
   begin
      return Ada.Strings.Unbounded.To_String (Local_Infinity_Path);
   end Infinity_Path;

   ------------------------
   -- Read_Configuration --
   ------------------------

   procedure Read_Configuration is
   begin
      Read_Infinity_Config
        (Tropos.Reader.Read_Config
           (Chaos.Paths.Config_File ("infinity.txt")));

      Chaos.Teams.Configure.Read_Config;
      Chaos.Powers.Configure.Read_Config;
      Chaos.Races.Configure.Read_Config;
      Chaos.Classes.Configure.Read_Config;
   end Read_Configuration;

   --------------------------
   -- Read_Infinity_Config --
   --------------------------

   procedure Read_Infinity_Config
     (Config : Tropos.Configuration)
   is
   begin
      Local_Infinity_Path :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (String'(Config.Get ("path")));
   end Read_Infinity_Config;

end Chaos.Configuration;
