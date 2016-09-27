with Chaos.Creatures.Db;

with Chaos.Classes.Create;

package body Chaos.Creatures.Quick is

   --------------------
   -- Quick_Creature --
   --------------------

   function Quick_Creature
     (Name  : String;
      Race  : Chaos.Races.Chaos_Race;
      Class : Chaos.Classes.Chaos_Class)
      return Chaos_Creature
   is

      procedure Create (Creature : in out Chaos_Creature_Record'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Creature : in out Chaos_Creature_Record'Class) is
      begin
         Creature.Initialize (Name);
         Creature.Add_Power (Chaos.Powers.Get ("basic-melee-attack"));
         Creature.Short_Name :=
           Ada.Strings.Unbounded.To_Unbounded_String (Name);
         Creature.Long_Name :=
           Ada.Strings.Unbounded.To_Unbounded_String (Name);
         Creature.Race := Race;
         Creature.Class := Class;
         Creature.Abilities :=
           Chaos.Classes.Create.Default_Abilities (Class);
         Creature.Level := 1;
         Creature.HP := Creature.Max_Hit_Points;
         Creature.Team := Chaos.Teams.Get ("pc");
      end Create;

   begin
      return Db.Create (Create'Access);
   end Quick_Creature;

end Chaos.Creatures.Quick;
