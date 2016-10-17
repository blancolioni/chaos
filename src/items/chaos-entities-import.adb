with Chaos.Resources.Itm;
with Chaos.Resources.Manager;

with Chaos.Logging;

with Chaos.Entities.Misc.Import;

package body Chaos.Entities.Import is

   -------------------
   -- Import_Entity --
   -------------------

   procedure Import_Entity (Code : String) is
      use Chaos.Resources.Itm;
   begin
      if Chaos.Resources.Manager.Resource_Exists
        (Chaos.Resources.To_Reference (Code), Chaos.Resources.Item_Resource)
      then
         declare
            Itm : Item_Resource renames
                    Item_Resource
                      (Chaos.Resources.Manager.Load_Resource
                         (Chaos.Resources.To_Reference (Code),
                          Chaos.Resources.Item_Resource).all);
         begin
            case Itm.Item_Type is
               when Books =>
                  Chaos.Logging.Log ("ITEM", "book: " & Code);
                  Chaos.Entities.Misc.Import.Import (Code, Itm);
               when others =>
                  Chaos.Logging.Log ("ITEM",
                                     Itm.Item_Type'Img
                                     & " " & Code);
                  Chaos.Entities.Misc.Import.Import (Code, Itm);
            end case;
         end;
      end if;
   end Import_Entity;

end Chaos.Entities.Import;
