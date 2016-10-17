with Chaos.Resources.Ids;
with Chaos.Resources.Manager;

package body Chaos.Identifiers.Import is

   ------------------------
   -- Import_Identifiers --
   ------------------------

   procedure Import_Identifiers (Group_Name : String) is
      Ids : Chaos.Resources.Ids.Ids_Resource'Class renames
                     Chaos.Resources.Ids.Ids_Resource'Class
                       (Chaos.Resources.Manager.Load_Resource
                          (Chaos.Resources.To_Reference (Group_Name),
                           Chaos.Resources.Identifier_Resource).all);
   begin
      for I in 1 .. Ids.Row_Count loop
         Add (Ids.Identifier (I), Ids.Value (I), Group_Name);
      end loop;
   end Import_Identifiers;

end Chaos.Identifiers.Import;
