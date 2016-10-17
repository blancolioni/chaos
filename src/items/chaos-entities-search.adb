with Chaos.Entities.Import;

package body Chaos.Entities.Search is

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Code : String)
      return Chaos_Entity
   is
   begin
      if not Exists (Code) then
         Chaos.Entities.Import.Import_Entity (Code);
      end if;

      if Exists (Code) then
         return Get (Code);
      else
         return null;
      end if;

   end Get_Entity;

end Chaos.Entities.Search;
