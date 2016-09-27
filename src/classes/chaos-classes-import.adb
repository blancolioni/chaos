with Chaos.Logging;

package body Chaos.Classes.Import is

   ------------------
   -- Import_Class --
   ------------------

   function Import_Class (Id : Natural) return Chaos_Class is
   begin
      case Id is
         when 1 =>
            return Get ("wizard");
         when 2 =>
            return Get ("fighter");
         when 155 =>
            return Get ("no-class");
         when 255 =>
            return Get ("no-class");
         when others =>
            Chaos.Logging.Log
              ("CLASS", "unknown class id:" & Id'Img);
            return Get ("no-class");
      end case;
   end Import_Class;

end Chaos.Classes.Import;
