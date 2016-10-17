private with Memor;

package Chaos.Entities.Misc is

   type Chaos_Misc_Entity_Record is
     new Chaos_Entity_Record with private;

   type Chaos_Misc_Entity is
     access constant Chaos_Misc_Entity_Record'Class;

private

   type Chaos_Misc_Entity_Record is
     new Chaos_Entity_Record with
      record
         null;
      end record;

   overriding function Object_Database
     (Entity : Chaos_Misc_Entity_Record)
      return Memor.Memor_Database;

end Chaos.Entities.Misc;
