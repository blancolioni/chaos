with Chaos.Creatures.Db;
with Chaos.Abilities.Able_Objects;
with Chaos.Defences.Defender_Objects;

package body Chaos.Creatures is

   package Ability_Properties is
     new Chaos.Abilities.Able_Objects (Chaos_Creature_Record);

   package Defence_Properties is
     new Chaos.Defences.Defender_Objects (Chaos_Creature_Record);

   function Last_Talked_To_By_Property
     (Object : Chaos.Objects.Root_Chaos_Object_Record'Class)
      return Lith.Objects.Object;

   function Num_Times_Talked_To_Property
     (Object : Chaos.Objects.Root_Chaos_Object_Record'Class)
      return Lith.Objects.Object;

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
      return Chaos.Entities.Weapons.Chaos_Weapon
   is
   begin
      return Chaos.Entities.Weapons.Chaos_Weapon
        (Creature.Equipment (Creature.Active_Weapon_Slot).Entity);
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

   --------------------
   -- Add_Properties --
   --------------------

   overriding procedure Add_Properties
     (Object : Chaos_Creature_Record)
   is
   begin
      Ability_Properties.Add_Ability_Properties (Object);
      Defence_Properties.Add_Defence_Properties (Object);
      Object.Add_Property ("last-talked-to-by",
                           Last_Talked_To_By_Property'Access);
      Object.Add_Property ("num-times-talked-to",
                           Num_Times_Talked_To_Property'Access);
   end Add_Properties;

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

   ------------------------------
   -- Change_Experience_Points --
   ------------------------------

   procedure Change_Experience_Points
     (Creature : in out Chaos_Creature_Record'Class;
      XP       : Integer)
   is
   begin
      Creature.XP := Creature.XP + XP;
   end Change_Experience_Points;

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

   ------------
   -- Exists --
   ------------

   function Exists
     (Identifier : String)
      return Boolean
   is
   begin
      return Db.Exists (Identifier);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get
     (Identifier : String)
      return Chaos_Creature
   is
   begin
      return Db.Get (Identifier);
   end Get;

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

   ----------
   -- Item --
   ----------

   overriding function Item
     (Creature : Chaos_Creature_Record;
      Index    : Positive)
      return Chaos.Items.Chaos_Item
   is
   begin
      return Creature.Inventory (Index);
   end Item;

   ----------
   -- Kill --
   ----------

   procedure Kill
     (Creature : in out Chaos_Creature_Record'Class)
   is
   begin
      Creature.Alive := False;
   end Kill;

   --------------------------------
   -- Last_Talked_To_By_Property --
   --------------------------------

   function Last_Talked_To_By_Property
     (Object : Chaos.Objects.Root_Chaos_Object_Record'Class)
      return Lith.Objects.Object
   is
      Creature : Chaos_Creature_Record'Class renames
                   Chaos_Creature_Record'Class (Object);
   begin
      if Creature.Last_Talked_To_By = null then
         return Lith.Objects.Nil;
      else
         return Creature.Last_Talked_To_By.To_Expression;
      end if;
   end Last_Talked_To_By_Property;

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

   ----------
   -- Mark --
   ----------

   overriding procedure Mark
     (Creature   : in out Chaos_Creature_Record;
      Mark_Value : not null access
        procedure (Value : in out Lith.Objects.Object))
   is
   begin
      Chaos.Objects.Root_Chaos_Object_Record (Creature).Mark (Mark_Value);
   end Mark;

   -----------
   -- Match --
   -----------

   function Match
     (Creature   : Chaos_Creature_Record'Class;
      Properties : Creature_Match)
      return Boolean
   is
      Ids : Creature_Object_Ids renames Creature.Object_Ids;
   begin
      return (Properties.Enemy_Ally = 0
              or else Properties.Enemy_Ally = Ids (EA_Id))
        and then (Properties.General = 0
                  or else Properties.General = Ids (General_Id))
        and then (Properties.Race = 0
                  or else Properties.Race = Ids (Race_Id))
        and then (Properties.Class = 0
                  or else Properties.Class = Ids (Class_Id))
        and then (Properties.Specific = 0
                  or else Properties.Specific = Ids (Specific_Id))
        and then (Properties.Gender = 0
                  or else Properties.Gender = Ids (Gender_Id))
        and then (Properties.Alignment = 0
                  or else Properties.Alignment = Ids (Alignment_Id));
   end Match;

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

   ----------------------------------
   -- Num_Times_Talked_To_Property --
   ----------------------------------

   function Num_Times_Talked_To_Property
     (Object : Chaos.Objects.Root_Chaos_Object_Record'Class)
      return Lith.Objects.Object
   is
   begin
      return Lith.Objects.To_Object
        (Chaos_Creature_Record'Class (Object).Num_Times_Talked_To);
   end Num_Times_Talked_To_Property;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Object : Chaos_Creature_Record)
      return Memor.Memor_Database
   is
      pragma Unreferenced (Object);
   begin
      return Db.Get_Database;
   end Object_Database;

   -------------------
   -- On_End_Dialog --
   -------------------

   overriding procedure On_End_Dialog
     (Creature : Chaos_Creature_Record)
   is
      procedure Update (C : in out Chaos_Creature_Record'Class);

      ------------
      -- Update --
      ------------

      procedure Update (C : in out Chaos_Creature_Record'Class) is
      begin
         C.Num_Times_Talked_To := C.Num_Times_Talked_To + 1;
      end Update;

   begin
      Creature.Update (Update'Access);
   end On_End_Dialog;

   ---------------------
   -- On_Start_Dialog --
   ---------------------

   procedure On_Start_Dialog
     (Creature_1, Creature_2 : Chaos_Creature)
   is
      procedure Update_Creature (C : in out Chaos_Creature_Record'Class);

      ---------------------
      -- Update_Creature --
      ---------------------

      procedure Update_Creature (C : in out Chaos_Creature_Record'Class) is
      begin
         if C.Identifier = Creature_1.Identifier then
            C.Last_Talked_To_By := Creature_2;
         else
            C.Last_Talked_To_By := Creature_1;
         end if;
      end Update_Creature;

   begin
      Creature_1.Update (Update_Creature'Access);
      Creature_2.Update (Update_Creature'Access);
   end On_Start_Dialog;

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

   overriding procedure Replace_Item
     (Creature : in out Chaos_Creature_Record;
      Index    : Positive;
      Item     : Chaos.Items.Chaos_Item)
   is
   begin
      Creature.Inventory (Index) := Item;
   end Replace_Item;

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
      Item    : Chaos.Items.Chaos_Item)
   is
   begin
      Creature.Equipment (Slot) := Item;
   end Set_Equipment;

   -------------------
   -- Set_Object_Id --
   -------------------

   procedure Set_Object_Id
     (Creature : in out Chaos_Creature_Record'Class;
      Name     : String;
      Value    : Natural)
   is
      Group : constant Creature_Object_Id :=
                Creature_Object_Id'Value
                  (Name & "_Id");
   begin
      Creature.Log (Group'Img & " :=" & Value'Img);
      Creature.Object_Ids (Group) := Value;
   end Set_Object_Id;

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
     (Creature : Chaos_Creature_Record'Class;
      Updater  : not null access
        procedure (Creature : in out Chaos_Creature_Record'Class))
   is
   begin
      Db.Update (Creature.Reference, Updater);
   end Update;

end Chaos.Creatures;
