with Tropos.Reader;

with Chaos.Paths;
with Chaos.Logging;

with Chaos.Races.Db;

with Chaos.Abilities.Configure;
with Chaos.Defences.Configure;

package body Chaos.Races.Configure is

   procedure Configure_Race
     (Config : Tropos.Configuration);

   --------------------
   -- Configure_Race --
   --------------------

   procedure Configure_Race
     (Config : Tropos.Configuration)
   is
      procedure Create
        (Race : in out Chaos_Race_Record'Class);

      ------------
      -- Create --
      ------------

      procedure Create
        (Race : in out Chaos_Race_Record'Class)
      is
      begin
         Race.Initialize (Config.Config_Name);
         if Config.Contains ("abilities") then
            Race.Abilities :=
              Chaos.Abilities.Configure.Configure_Ability_Changes
                (Config);
         end if;
         if Config.Contains ("defences") then
            Race.Defences :=
              Chaos.Defences.Configure.Configure_Defence_Changes
                (Config);
         end if;

         if Config.Contains ("size") then
            begin
               Race.Size :=
                 Chaos.Sizes.Chaos_Size'Value
                   (Config.Get ("size"));
            exception
               when Constraint_Error =>
                  Chaos.Logging.Log
                    ("CONFIG",
                     "in configuration for race '"
                     & Race.Identifier
                     & "': invalid size '" & Config.Get ("size") & "'");
            end;
         end if;

         if Config.Contains ("speed") then
            begin
               Race.Speed :=
                 Chaos.Speed.Chaos_Speed'Value
                   (Config.Get ("speed"));
            exception
               when Constraint_Error =>
                  Chaos.Logging.Log
                    ("CONFIG",
                     "in configuration for race '"
                     & Race.Identifier
                     & "': invalid speed '" & Config.Get ("speed") & "'");
            end;
         end if;

         if Config.Contains ("vision") then
            begin
               Race.Vision :=
                 Chaos.Vision.Chaos_Vision'Value
                   (Config.Get ("vision"));
            exception
               when Constraint_Error =>
                  Chaos.Logging.Log
                    ("CONFIG",
                     "in configuration for race '"
                     & Race.Identifier
                     & "': invalid vision '" & Config.Get ("vision") & "'");
            end;
         end if;

         Chaos.Logging.Log
           ("RACE", "created " & Race.Display_Name);
      end Create;

   begin
      Chaos.Races.Db.Create
        (Create'Access);
   end Configure_Race;

   -----------------
   -- Read_Config --
   -----------------

   procedure Read_Config is
   begin
      Tropos.Reader.Read_Config
        (Chaos.Paths.Config_File ("races"),
         "txt",
         Configure_Race'Access);
   end Read_Config;

end Chaos.Races.Configure;
