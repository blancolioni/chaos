private with Ada.Strings.Unbounded;

private with Memor;

with Chaos.Coins;
with Chaos.Abilities;
with Chaos.Defences;
with Chaos.Levels;

with Chaos.Objects;

with Chaos.Classes;
with Chaos.Races;

with Chaos.Alignment;
with Chaos.Teams;
with Chaos.Vision;

with Chaos.Equipment;
with Chaos.Items.Weapons;
with Chaos.Things;

with Chaos.Powers;
with Chaos.Dialog;

package Chaos.Creatures is

   Max_Inventory_Items : constant := 40;
   type Inventory_Count is range 0 .. Max_Inventory_Items;
   subtype Inventory_Index is Inventory_Count range 1 .. Inventory_Count'Last;

   type Chaos_Creature_Record is
     new Chaos.Objects.Root_Localised_Object_Record
     and Chaos.Abilities.Ability_Interface
     and Chaos.Defences.Defence_Interface
     and Chaos.Races.Chaos_Race_Interface
     and Chaos.Classes.Chaos_Class_Interface
     and Chaos.Levels.Chaos_Level_Interface
     and Chaos.Vision.Chaos_Vision_Interface
     and Chaos.Powers.Powered_Interface
   with private;

   overriding function Ability_Score
     (Creature : Chaos_Creature_Record;
      Ability  : Chaos.Abilities.Ability)
      return Chaos.Abilities.Ability_Score_Range;

   overriding function Defence_Score
     (Creature : Chaos_Creature_Record;
      Defence  : Chaos.Defences.Defence)
      return Chaos.Defences.Defence_Score_Range;

   overriding function Race
     (Creature : Chaos_Creature_Record)
      return Chaos.Races.Chaos_Race;

   overriding function Class
     (Creature : Chaos_Creature_Record)
      return Chaos.Classes.Chaos_Class;

   overriding function Level
     (Creature : Chaos_Creature_Record)
      return Chaos.Levels.Chaos_Level;

   overriding procedure Add_Power
     (Creature : in out Chaos_Creature_Record;
      Power    : Chaos.Powers.Chaos_Power);

   overriding function Power_Count
     (Creature : Chaos_Creature_Record)
      return Natural;

   overriding function Get_Power
     (Creature : Chaos_Creature_Record;
      Index    : Positive)
      return Chaos.Powers.Chaos_Power;

   overriding function Vision
     (Creature : Chaos_Creature_Record)
      return Chaos.Vision.Chaos_Vision
   is (Creature.Race.Vision);

   function Long_Name
     (Creature : Chaos_Creature_Record'Class)
      return String;

   function Short_Name
     (Creature : Chaos_Creature_Record'Class)
      return String;

   function Individual
     (Creature : Chaos_Creature_Record'Class)
      return Boolean;

   function Alive
     (Creature : Chaos_Creature_Record'Class)
      return Boolean;

   function Max_Hit_Points
     (Creature : Chaos_Creature_Record'Class)
      return Natural;

   function Current_Hit_Points
     (Creature : Chaos_Creature_Record'Class)
      return Natural;

   procedure Set_Current_Hit_Points
     (Creature   : in out Chaos_Creature_Record'Class;
      Hit_Points : Natural)
     with Pre => Creature.Individual,
     Post => Creature.Current_Hit_Points = Hit_Points;

   function Active_Weapon_Slot
     (Creature : Chaos_Creature_Record'Class)
      return Chaos.Equipment.Chaos_Equipment_Slot;

   procedure Set_Active_Weapon_Slot
     (Creature : in out Chaos_Creature_Record'Class;
      Slot     : Chaos.Equipment.Weapon_Slot);

   function Active_Weapon
     (Creature : Chaos_Creature_Record'Class)
      return Chaos.Items.Weapons.Chaos_Weapon;

   procedure Set_Equipment
     (Creature : in out Chaos_Creature_Record'Class;
      Slot     : Chaos.Equipment.Chaos_Equipment_Slot;
      Thing    : Chaos.Things.Chaos_Thing)
     with Pre => Thing.Equipment_Slot_OK (Slot);

   procedure Set_Inventory
     (Creature : in out Chaos_Creature_Record'Class;
      Index    : Inventory_Index;
      Thing    : Chaos.Things.Chaos_Thing);

   function Inventory
     (Creature : in out Chaos_Creature_Record'Class;
      Index    : Inventory_Index)
      return Chaos.Things.Chaos_Thing;

   function Has_Dialog
     (Creature : Chaos_Creature_Record'Class)
      return Boolean;

   function Dialog
     (Creature : Chaos_Creature_Record'Class)
      return Chaos.Dialog.Chaos_Dialog
     with Pre => Creature.Has_Dialog;

   function Team
     (Creature : Chaos_Creature_Record'Class)
      return Chaos.Teams.Chaos_Team;

   procedure Kill
     (Creature : in out Chaos_Creature_Record'Class)
     with Pre => Creature.Individual and then Creature.Alive,
     Post => not Creature.Alive;

   function Animation_Id
     (Creature : Chaos_Creature_Record'Class)
      return Natural;

   type Chaos_Creature is access constant Chaos_Creature_Record'Class;

   procedure Update
     (Creature : Chaos_Creature;
      Updater  : not null access
        procedure (Creature : in out Chaos_Creature_Record'Class));

private

   type Creature_Color_Part is
     (Metal, Minor, Major, Skin, Leather, Armour, Hair);

   type Creature_Color_Map is
     array (Creature_Color_Part) of Natural;

   type Creature_Equipment is
     array (Chaos.Equipment.Chaos_Equipment_Slot) of Chaos.Things.Chaos_Thing;

   type Inventory_Item_Array is
     array (Inventory_Index) of Chaos.Things.Chaos_Thing;

   type Chaos_Creature_Record is
     new Chaos.Objects.Root_Localised_Object_Record
     and Chaos.Abilities.Ability_Interface
     and Chaos.Defences.Defence_Interface
     and Chaos.Races.Chaos_Race_Interface
     and Chaos.Classes.Chaos_Class_Interface
     and Chaos.Levels.Chaos_Level_Interface
     and Chaos.Vision.Chaos_Vision_Interface
     and Chaos.Powers.Powered_Interface with
      record
         Individual    : Boolean;
         Alive         : Boolean;
         Short_Name    : Ada.Strings.Unbounded.Unbounded_String;
         Long_Name     : Ada.Strings.Unbounded.Unbounded_String;
         Race          : Chaos.Races.Chaos_Race;
         Class         : Chaos.Classes.Chaos_Class;
         Abilities     : Chaos.Abilities.Ability_Scores;
         Level         : Chaos.Levels.Chaos_Level;
         HP            : Natural;
         Powers        : Chaos.Powers.Power_Collection;
         Alignment     : Chaos.Alignment.Chaos_Alignment;
         Team          : Chaos.Teams.Chaos_Team;
         Dialog        : Chaos.Dialog.Chaos_Dialog;
         Animation_Id  : Natural := 0;
         Color_Map     : Creature_Color_Map;
         Cash          : Chaos.Coins.Coins_Type;
         Equipment     : Creature_Equipment := (others => null);
         Inventory     : Inventory_Item_Array := (others => null);
         Active_Weapon : Chaos.Equipment.Weapon_Slot :=
                           Chaos.Equipment.Weapon_1;
      end record;

   overriding function Object_Database
     (Object : Chaos_Creature_Record)
      return Memor.Memor_Database;

   overriding procedure Add_Properties
     (Object : Chaos_Creature_Record);

end Chaos.Creatures;
