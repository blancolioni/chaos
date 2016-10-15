with Chaos.Resources.Manager;

with Chaos.Creatures;
with Chaos.Items.Weapons;

with Chaos.Creatures.Import;

package body Chaos.Objects.Search is

   -----------------
   -- Find_Object --
   -----------------

   function Find_Object
     (Code     : String;
      Creature : Boolean := True;
      Item     : Boolean := True)
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
               use type Chaos.Creatures.Chaos_Creature;
               Creature : constant Chaos.Creatures.Chaos_Creature :=
                            Chaos.Creatures.Import.Import_Creature
                              (Code);
            begin
               return Chaos_Object (Creature);
            end;
         end if;
      end if;

      if Item then
         if Chaos.Items.Weapons.Exists (Code) then
            return Chaos_Object (Chaos.Items.Weapons.Get (Code));
         end if;
      end if;

      return null;
   end Find_Object;

end Chaos.Objects.Search;
