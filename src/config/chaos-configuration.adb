with Tropos.Reader;

with Chaos.Paths;
with Chaos.Classes.Configure;
with Chaos.Items.Weapons.Configure;
with Chaos.Localisation.Configure;
with Chaos.Powers.Configure;
with Chaos.Races.Configure;
with Chaos.Teams.Configure;

package body Chaos.Configuration is

   Chaos_Config : Tropos.Configuration;

   ------------------------
   -- Read_Configuration --
   ------------------------

   procedure Read_Configuration is
   begin
      Chaos_Config :=
        Tropos.Reader.Read_Config
          (Chaos.Paths.Config_File ("chaos.config"));

      Chaos.Localisation.Configure.Read_Local_Text
        (Language => Chaos_Config.Get ("language", "english"));
      Chaos.Teams.Configure.Read_Config;
      Chaos.Items.Weapons.Configure.Read_Config;
      Chaos.Powers.Configure.Read_Config;
      Chaos.Races.Configure.Read_Config;
      Chaos.Classes.Configure.Read_Config;
   end Read_Configuration;

end Chaos.Configuration;
