with Chaos.Creatures.Db;
with Chaos.Creatures.Reports;

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
         for Ability in Creature.Abilities'Range loop
            Chaos.Abilities.Apply (Creature.Abilities (Ability),
                                   Creature.Race.Ability_Bonus (Ability));
         end loop;

         Creature.Level := 1;
         Creature.HP := Creature.Max_Hit_Points;
         Creature.Team := Chaos.Teams.Get ("pc");
         Creature.Color_Map :=
           (Metal => 25, Minor => 41, Major => 47,
            Skin  => 12, Leather => 91, Armour => 25, Hair => 91);
         Creature.Cash := Chaos.Coins.GP (100);
         Creature.Set_Equipment
           (Chaos.Equipment.Weapon_1,
            Chaos.Items.Create
              (Chaos.Entities.Weapons.Get ("staf01")));

         Creature.Set_Active_Weapon_Slot (Chaos.Equipment.Weapon_1);
      end Create;

      Creature : constant Chaos_Creature := Db.Create (Create'Access);
   begin
      Chaos.Creatures.Reports.Report (Creature);
      return Creature;
   end Quick_Creature;

end Chaos.Creatures.Quick;
