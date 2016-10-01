with Chaos.Classes.Configure;
with Chaos.Powers.Configure;
with Chaos.Races.Configure;
with Chaos.Teams.Configure;

package body Chaos.Configuration is

   ------------------------
   -- Read_Configuration --
   ------------------------

   procedure Read_Configuration is
   begin
      Chaos.Teams.Configure.Read_Config;
      Chaos.Powers.Configure.Read_Config;
      Chaos.Races.Configure.Read_Config;
      Chaos.Classes.Configure.Read_Config;
   end Read_Configuration;

end Chaos.Configuration;
