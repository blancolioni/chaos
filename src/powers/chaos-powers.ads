private with Ada.Containers.Vectors;

private with Memor;

private with Lith.Objects;

private with Chaos.Expressions.Enumerated;

limited with Chaos.Actors;

with Chaos.Expressions;

with Chaos.Objects;
with Chaos.Locations;

with Chaos.Actions;

package Chaos.Powers is

   type Power_Class is (Attack, Utility);

   type Power_Source is (Arcane, Divine, Martial);

   type Power_Use_Class is (At_Will, Encounter, Daily);

   type Power_Attack_Target is (One_Creature,
                                Blast, Burst, Wall);

   type Power_Implement is (None, Weapon, Arcane, Divine);

   type Power_Damage_Type is
     (Normal,
      Acid, Cold, Fire, Force, Lightning,
      Necrotic, Poison, Psychic, Radiant, Thunder);

   type Chaos_Power_Record is
     new Chaos.Objects.Root_Localised_Object_Record
   with private;

   function Class
     (Power : Chaos_Power_Record'Class)
      return Power_Class;

   function Source
     (Power : Chaos_Power_Record'Class)
      return Power_Source;

   function Use_Class
     (Power : Chaos_Power_Record'Class)
      return Power_Use_Class;

   function Attack_Target
     (Power : Chaos_Power_Record'Class)
      return Power_Attack_Target;

   function Short_Range (Power : Chaos_Power_Record'Class) return Natural;
   function Long_Range (Power : Chaos_Power_Record'Class) return Natural;

   procedure Attack
     (Power : Chaos_Power_Record'Class;
      Attacker : not null access constant
        Chaos.Actors.Chaos_Actor_Record'Class;
      Defender : not null access constant
        Chaos.Actors.Chaos_Actor_Record'Class);

   procedure Attack
     (Power    : Chaos_Power_Record'Class;
      Attacker : not null access constant
        Chaos.Actors.Chaos_Actor_Record'Class;
      Target   : Chaos.Locations.Square_Location);

   type Chaos_Power is access constant Chaos_Power_Record'Class;

   function Get (Identifier : String) return Chaos_Power;

   type Powered_Interface is limited interface;

   procedure Add_Power
     (Powered : in out Powered_Interface;
      Power   : Chaos_Power)
   is abstract;

   function Power_Count
     (Powered : Powered_Interface)
      return Natural
      is abstract;

   function Get_Power
     (Powered : Powered_Interface;
      Index   : Positive)
      return Chaos_Power
      is abstract;

   type Power_Collection is
     new Powered_Interface with private;

   overriding procedure Add_Power
     (Collection : in out Power_Collection;
      Power      : Chaos_Power);

   overriding function Power_Count
     (Collection : Power_Collection)
      return Natural;

   overriding function Get_Power
     (Collection : Power_Collection;
      Index      : Positive)
      return Chaos_Power;

private

   type Chaos_Power_Record is
     new Chaos.Objects.Root_Localised_Object_Record with
      record
         Class        : Power_Class;
         Source       : Power_Source;
         Use_Class    : Power_Use_Class;
         Implement    : Power_Implement;
         Action       : Chaos.Actions.Chaos_Action;
         Target       : Power_Attack_Target;
         Target_Size  : Natural := 1;
         Short_Range  : Natural := 1;
         Long_Range   : Natural := 1;
         Attack       : Lith.Objects.Object := Lith.Objects.Nil;
         Defence      : Lith.Objects.Object := Lith.Objects.Nil;
         Hit_Effects  : Lith.Objects.Object := Lith.Objects.Nil;
         Miss_Effects : Lith.Objects.Object := Lith.Objects.Nil;
         Effects      : Lith.Objects.Object := Lith.Objects.Nil;
      end record;

   overriding function Object_Database
     (Object : Chaos_Power_Record)
      return Memor.Memor_Database;

   overriding procedure Add_Properties
     (Object : Chaos_Power_Record);

   overriding procedure Mark
     (Power      : in out Chaos_Power_Record;
      Mark_Value : not null access
        procedure (Value : in out Lith.Objects.Object));

   package Power_Vectors is new Ada.Containers.Vectors (Positive, Chaos_Power);

   type Power_Collection is
     new Powered_Interface with
      record
         Powers : Power_Vectors.Vector;
      end record;

   package Power_Source_Expressions is
     new Chaos.Expressions.Enumerated (Power_Source);

   package Power_Use_Expressions is
     new Chaos.Expressions.Enumerated (Power_Use_Class);

   package Power_Target_Expressions is
     new Chaos.Expressions.Enumerated (Power_Attack_Target);

   package Power_Implement_Expressions is
     new Chaos.Expressions.Enumerated (Power_Implement);

   package Power_Damage_Type_Expressions is
     new Chaos.Expressions.Enumerated (Power_Damage_Type);

   function Is_Power_Damage_Type
     (E : Lith.Objects.Object)
      return Boolean
   is (Power_Damage_Type_Expressions.Is_Enum (E));

   function Get_Power_Damage_Type
     (E : Lith.Objects.Object)
      return Power_Damage_Type
   is (Power_Damage_Type_Expressions.To_Enum (E));

   function Attack_Setting
     (Power : Chaos_Power_Record'Class)
      return String
   is (Power.Global_Setting_Name ("attack"));

   function Defence_Setting
     (Power : Chaos_Power_Record'Class)
      return String
   is (Power.Global_Setting_Name ("defence"));

   function Hit_DamageSetting
     (Power : Chaos_Power_Record'Class)
      return String
   is (Power.Global_Setting_Name ("hit"));

   function Miss_Setting
     (Power : Chaos_Power_Record'Class)
      return String
   is (Power.Global_Setting_Name ("miss"));

   function Effect_Setting
     (Power : Chaos_Power_Record'Class)
      return String
   is (Power.Global_Setting_Name ("effect"));

end Chaos.Powers;
