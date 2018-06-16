with Chaos.Resources.Manager;

with Chaos.Creatures;
with Chaos.Entities.Search;
with Chaos.Features;

with Chaos.Creatures.Import;

package body Chaos.Objects.Search is

   --------------------------
   -- Find_Creature_Object --
   --------------------------

   function Find_Creature_Object
     (Code : String)
      return Chaos_Object
   is
   begin
      return Find_Object (Code, Creature => True, Entity => False);
   end Find_Creature_Object;

   ------------------------
   -- Find_Entity_Object --
   ------------------------

   function Find_Entity_Object
     (Code : String)
      return Chaos_Object
   is
   begin
      return Find_Object (Code, Creature => False, Entity => True);
   end Find_Entity_Object;

   -----------------
   -- Find_Object --
   -----------------

   function Find_Object
     (Code       : String;
      Creature   : Boolean := True;
      Entity     : Boolean := True;
      Feature    : Boolean := True)
      return Chaos_Object
   is
   begin
      if Creature then
         if Chaos.Creatures.Exists (Code) then
            return Chaos_Object (Chaos.Creatures.Get (Code));
         elsif Chaos.Resources.Manager.Resource_Exists
           (Chaos.Resources.To_Reference (Code),
            Chaos.Resources.Creature_Resource)
         then
            declare
               Creature : constant Chaos.Creatures.Chaos_Creature :=
                            Chaos.Creatures.Import.Import_Creature
                              (Code);
            begin
               return Chaos_Object (Creature);
            end;
         end if;
      end if;

      if Entity then
         declare
            use type Chaos.Entities.Chaos_Entity;
            Result : constant Chaos.Entities.Chaos_Entity :=
                       Chaos.Entities.Search.Get_Entity (Code);
         begin
            if Result /= null then
               return Chaos_Object (Result);
            end if;
         end;
      end if;

      if Feature then
         if Chaos.Features.Exists (Code) then
            return Chaos_Object (Chaos.Features.Get (Code));
         end if;
      end if;

      return null;
   end Find_Object;

end Chaos.Objects.Search;
