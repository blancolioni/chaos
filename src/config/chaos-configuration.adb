with Tropos.Reader;

with Chaos.Paths;
with Chaos.Classes.Configure;
with Chaos.Entities.Weapons.Configure;
with Chaos.Localisation.Configure;
with Chaos.Powers.Configure;
with Chaos.Races.Configure;
with Chaos.Teams.Configure;

with Chaos.Areas.Primitives;
with Chaos.Game.Primitives;
with Chaos.Expressions;
with Chaos.Objects.Primitives;
with Chaos.Creatures.Primitives;

with Chaos.Expressions.Import.Actions;
with Chaos.Expressions.Import.Objects;
with Chaos.Expressions.Import.Triggers;

with Chaos.Identifiers.Import;

package body Chaos.Configuration is

   Chaos_Config : Tropos.Configuration;

   ------------------------
   -- Read_Configuration --
   ------------------------

   procedure Read_Configuration is
   begin

      Chaos.Expressions.Create_Environment;

      Chaos.Identifiers.Import.Import_Identifiers ("object");
      Chaos.Identifiers.Import.Import_Identifiers ("ea");
      Chaos.Identifiers.Import.Import_Identifiers ("reaction");

      Chaos.Areas.Primitives.Create_Primitives;
      Chaos.Creatures.Primitives.Add_Primitives;
      Chaos.Game.Primitives.Add_Primitives;
      Chaos.Objects.Primitives.Add_Primitives;

      Chaos.Expressions.Import.Triggers.Load_Triggers;
      Chaos.Expressions.Import.Actions.Load_Actions;
      Chaos.Expressions.Import.Objects.Load_Objects;

      Chaos_Config :=
        Tropos.Reader.Read_Config
          (Chaos.Paths.Config_File ("chaos.config"));

      Chaos.Localisation.Configure.Read_Local_Text
        (Language => Chaos_Config.Get ("language", "english"));
      Chaos.Teams.Configure.Read_Config;
      Chaos.Entities.Weapons.Configure.Read_Config;
      Chaos.Powers.Configure.Read_Config;
      Chaos.Races.Configure.Read_Config;
      Chaos.Classes.Configure.Read_Config;
   end Read_Configuration;

end Chaos.Configuration;
