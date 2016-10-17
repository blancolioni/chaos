with Chaos.Entities.Misc.Db;

with Chaos.Logging;

package body Chaos.Entities.Misc.Import is

   ------------
   -- Import --
   ------------

   procedure Import
     (Code     : String;
      Resource : Chaos.Resources.Itm.Item_Resource'Class)
   is
      procedure Create (Entity : in out Chaos_Misc_Entity_Record'Class)
      is null;

      procedure Configure (Entity : in out Chaos_Misc_Entity_Record'Class);

      ---------------
      -- Configure --
      ---------------

      procedure Configure (Entity : in out Chaos_Misc_Entity_Record'Class) is
         use Chaos.Localisation;
      begin
         Entity.Initialize
           (Code, False,
            Resource.Identified_Name, Resource.Unidentified_Name,
            Resource.Identified_Desc, Resource.Unidentified_Desc);
      end Configure;

      Entity : constant Chaos_Misc_Entity :=
                   Db.Create (Create'Access);
   begin
      Entity.Save_Object;
      Db.Update (Entity.Reference, Configure'Access);
      Entity.Define_Object;
      Register_Entity (Code, Chaos_Entity (Entity));
      Chaos.Logging.Log
        ("ENTITY",
         Code & " "
         & Chaos.Localisation.Indexed_Text (Entity.Identified_Name));
   end Import;

end Chaos.Entities.Misc.Import;
