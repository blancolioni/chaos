with Chaos.Logging;

package body Chaos.Races.Import is

   -----------------
   -- Import_Race --
   -----------------

   function Import_Race (Id : Natural) return Chaos_Race is
   begin
      case Id is
         when 1 =>
            return Get ("human");
         when 2 =>
            return Get ("elf");
         when 255 =>
            return Get ("no-race");
         when others =>
            Chaos.Logging.Log
              ("RACE", "unknown race id:" & Id'Img);
            return Get ("human");
      end case;
   end Import_Race;

end Chaos.Races.Import;
