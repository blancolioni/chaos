with Chaos.Creatures.Db;

package body Chaos.Creatures is

   -------------------
   -- Ability_Score --
   -------------------

   overriding function Ability_Score
     (Creature : Chaos_Creature_Record;
      Ability  : Chaos.Abilities.Ability)
      return Chaos.Abilities.Ability_Score_Range
   is
   begin
      return Creature.Abilities (Ability);
   end Ability_Score;

   -------------------
   -- Active_Weapon --
   -------------------

   function Active_Weapon
     (Creature : Chaos_Creature_Record'Class)
      return Chaos.Items.Weapons.Chaos_Weapon
   is
   begin
      return Chaos.Items.Weapons.Chaos_Weapon
        (Creature.Equipment (Creature.Active_Weapon_Slot).Item);
   end Active_Weapon;

   ------------------------
   -- Active_Weapon_Slot --
   ------------------------

   function Active_Weapon_Slot
     (Creature : Chaos_Creature_Record'Class)
      return Chaos.Equipment.Chaos_Equipment_Slot
   is
   begin
      return Creature.Active_Weapon;
   end Active_Weapon_Slot;

   ---------------
   -- Add_Power --
   ---------------

   overriding procedure Add_Power
     (Creature : in out Chaos_Creature_Record;
      Power    : Chaos.Powers.Chaos_Power)
   is
   begin
      Creature.Powers.Add_Power (Power);
   end Add_Power;

   -----------
   -- Alive --
   -----------

   function Alive
     (Creature : Chaos_Creature_Record'Class)
      return Boolean
   is
   begin
      return Creature.Alive;
   end Alive;

   ------------------
   -- Animation_Id --
   ------------------

   function Animation_Id
     (Creature : Chaos_Creature_Record'Class)
      return Natural
   is
   begin
      return Creature.Animation_Id;
   end Animation_Id;

   -----------
   -- Class --
   -----------

   overriding function Class
     (Creature : Chaos_Creature_Record)
      return Chaos.Classes.Chaos_Class
   is
   begin
      return Creature.Class;
   end Class;

   ------------------------
   -- Current_Hit_Points --
   ------------------------

   function Current_Hit_Points
     (Creature : Chaos_Creature_Record'Class)
      return Natural
   is
   begin
      return Creature.HP;
   end Current_Hit_Points;

   -------------------
   -- Defence_Score --
   -------------------

   overriding function Defence_Score
     (Creature : Chaos_Creature_Record;
      Defence  : Chaos.Defences.Defence)
      return Chaos.Defences.Defence_Score_Range
   is
      use all type Chaos.Defences.Defence;
      use type Chaos.Defences.Defence_Score_Range;
      use type Chaos.Abilities.Ability_Score_Range;
      Base : constant Integer :=
               10 + Natural (Creature.Level) / 2;
      Bonus : Chaos.Abilities.Ability_Bonus_Range;
      Class_Bonus : Chaos.Defences.Defence_Score_Change := 0;
      Ability_1 : Chaos.Abilities.Ability;
      Ability_2 : Chaos.Abilities.Ability;
   begin
      case Defence is
         when Fort =>
            Ability_1 := Chaos.Abilities.Str;
            Ability_2 := Chaos.Abilities.Con;
         when Refl =>
            Ability_1 := Chaos.Abilities.Dex;
            Ability_2 := Chaos.Abilities.Int;
         when Will =>
            Ability_1 := Chaos.Abilities.Wis;
            Ability_2 := Chaos.Abilities.Cha;
         when AC =>
            Ability_1 := Chaos.Abilities.Dex;
            Ability_2 := Chaos.Abilities.Int;
      end case;

      if Creature.Ability_Score (Ability_1)
        > Creature.Ability_Score (Ability_2)
      then
         Bonus := Creature.Ability_Bonus (Ability_1);
      else
         Bonus := Creature.Ability_Bonus (Ability_2);
      end if;

      Class_Bonus := Creature.Class_Defence_Bonus (Defence);

      return Chaos.Defences.Defence_Score_Range
        (Base + Integer (Bonus) + Integer (Class_Bonus));
   end Defence_Score;

   ------------
   -- Dialog --
   ------------

   function Dialog
     (Creature : Chaos_Creature_Record'Class)
      return Chaos.Dialog.Chaos_Dialog
   is
   begin
      return Creature.Dialog;
   end Dialog;

   ---------------
   -- Get_Power --
   ---------------

   overriding function Get_Power
     (Creature : Chaos_Creature_Record;
      Index    : Positive)
      return Chaos.Powers.Chaos_Power
   is
   begin
      return Creature.Powers.Get_Power (Index);
   end Get_Power;

   ----------------
   -- Has_Dialog --
   ----------------

   function Has_Dialog
     (Creature : Chaos_Creature_Record'Class)
      return Boolean
   is
      use type Chaos.Dialog.Chaos_Dialog;
   begin
      return Creature.Dialog /= null;
   end Has_Dialog;

   ----------------
   -- Individual --
   ----------------

   function Individual
     (Creature : Chaos_Creature_Record'Class)
      return Boolean
   is
   begin
      return Creature.Individual;
   end Individual;

   ---------------
   -- Inventory --
   ---------------

   function Inventory
     (Creature : in out Chaos_Creature_Record'Class;
      Index    : Inventory_Index)
      return Chaos.Things.Chaos_Thing
   is
   begin
      return Creature.Inventory (Index);
   end Inventory;

   ----------
   -- Kill --
   ----------

   procedure Kill
     (Creature : in out Chaos_Creature_Record'Class)
   is
   begin
      Creature.Alive := False;
   end Kill;

   -----------
   -- Level --
   -----------

   overriding function Level
     (Creature : Chaos_Creature_Record)
      return Chaos.Levels.Chaos_Level
   is
   begin
      return Creature.Level;
   end Level;

   ---------------
   -- Long_Name --
   ---------------

   function Long_Name
     (Creature : Chaos_Creature_Record'Class)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Creature.Long_Name);
   end Long_Name;

   --------------------
   -- Max_Hit_Points --
   --------------------

   function Max_Hit_Points
     (Creature : Chaos_Creature_Record'Class)
      return Natural
   is
      use type Chaos.Levels.Chaos_Level;
      use type Chaos.Classes.Chaos_Class;
      Result : Natural := Natural (Creature.Abilities (Chaos.Abilities.Con));
   begin
      if Creature.Class /= null then
         if Creature.Level > 0 then
            Result := Result + Creature.Base_Hit_Points;
            Result := Result
              + Natural (Creature.Level - 1) *
              (Creature.Level_Hit_Points
               + Integer'Max
                 (0, Integer (Creature.Ability_Bonus (Chaos.Abilities.Con))));
         end if;
      end if;
      return Result;
   end Max_Hit_Points;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Object : Chaos_Creature_Record)
      return Memor.Root_Database_Type'Class
   is
      pragma Unreferenced (Object);
   begin
      return Db.Get_Database;
   end Object_Database;

   -----------------
   -- Power_Count --
   -----------------

   overriding function Power_Count
     (Creature : Chaos_Creature_Record)
      return Natural
   is
   begin
      return Creature.Powers.Power_Count;
   end Power_Count;

   ----------
   -- Race --
   ----------

   overriding function Race
     (Creature : Chaos_Creature_Record)
      return Chaos.Races.Chaos_Race
   is
   begin
      return Creature.Race;
   end Race;

   ----------------------------
   -- Set_Active_Weapon_Slot --
   ----------------------------

   procedure Set_Active_Weapon_Slot
     (Creature : in out Chaos_Creature_Record'Class;
      Slot     : Chaos.Equipment.Weapon_Slot)
   is
   begin
      Creature.Active_Weapon := Slot;
   end Set_Active_Weapon_Slot;

   ----------------------------
   -- Set_Current_Hit_Points --
   ----------------------------

   procedure Set_Current_Hit_Points
     (Creature   : in out Chaos_Creature_Record'Class;
      Hit_Points : Natural)
   is
   begin
      Creature.HP := Hit_Points;
   end Set_Current_Hit_Points;

   -------------------
   -- Set_Equipment --
   -------------------

   procedure Set_Equipment
     (Creature : in out Chaos_Creature_Record'Class;
      Slot     : Chaos.Equipment.Chaos_Equipment_Slot;
      Thing    : Chaos.Things.Chaos_Thing)
   is
   begin
      Creature.Equipment (Slot) := Thing;
   end Set_Equipment;

   -------------------
   -- Set_Inventory --
   -------------------

   procedure Set_Inventory
     (Creature : in out Chaos_Creature_Record'Class;
      Index    : Inventory_Index;
      Thing    : Chaos.Things.Chaos_Thing)
   is
   begin
      Creature.Inventory (Index) := Thing;
   end Set_Inventory;

   ----------------
   -- Short_Name --
   ----------------

   function Short_Name
     (Creature : Chaos_Creature_Record'Class)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Creature.Short_Name);
   end Short_Name;

   ----------
   -- Team --
   ----------

   function Team
     (Creature : Chaos_Creature_Record'Class)
      return Chaos.Teams.Chaos_Team
   is
   begin
      return Creature.Team;
   end Team;

   ------------
   -- Update --
   ------------

   procedure Update
     (Creature : Chaos_Creature;
      Updater  : not null access
        procedure (Creature : in out Chaos_Creature_Record'Class))
   is
   begin
      Db.Update (Creature.Reference, Updater);
   end Update;

end Chaos.Creatures;
