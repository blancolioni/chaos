private with Ada.Strings.Unbounded;

private with Memor;

with Chaos.Abilities;
with Chaos.Defences;
with Chaos.Levels;

with Chaos.Objects;

with Chaos.Classes;
with Chaos.Races;

with Chaos.Alignment;
with Chaos.Teams;
with Chaos.Vision;

with Chaos.Powers;
with Chaos.Dialog;

package Chaos.Creatures is

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

   type Chaos_Creature is access constant Chaos_Creature_Record'Class;

   procedure Update
     (Creature : Chaos_Creature;
      Updater  : not null access
        procedure (Creature : in out Chaos_Creature_Record'Class));

private

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
         Individual : Boolean;
         Alive      : Boolean;
         Short_Name : Ada.Strings.Unbounded.Unbounded_String;
         Long_Name  : Ada.Strings.Unbounded.Unbounded_String;
         Race       : Chaos.Races.Chaos_Race;
         Class      : Chaos.Classes.Chaos_Class;
         Abilities  : Chaos.Abilities.Ability_Scores;
         Level      : Chaos.Levels.Chaos_Level;
         HP         : Natural;
         Powers     : Chaos.Powers.Power_Collection;
         Alignment  : Chaos.Alignment.Chaos_Alignment;
         Team       : Chaos.Teams.Chaos_Team;
         Dialog     : Chaos.Dialog.Chaos_Dialog;
      end record;

   overriding function Object_Database
     (Object : Chaos_Creature_Record)
      return Memor.Root_Database_Type'Class;

end Chaos.Creatures;
