with Chaos.Entities.Misc.Db;

package body Chaos.Entities.Misc is

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Entity : Chaos_Misc_Entity_Record)
      return Memor.Memor_Database
   is
      pragma Unreferenced (Entity);
   begin
      return Db.Get_Database;
   end Object_Database;

end Chaos.Entities.Misc;
