private with Ada.Containers.Vectors;

private with Memor;

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

   type Power_Implement is (None, Weapon, Arcane);

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

   package Power_Effect_Vectors is
     new Ada.Containers.Vectors
       (Positive, Chaos.Expressions.Chaos_Expression,
        Chaos.Expressions."=");

   type Chaos_Power_Record is
     new Chaos.Objects.Root_Localised_Object_Record with
      record
         Class       : Power_Class;
         Source      : Power_Source;
         Use_Class   : Power_Use_Class;
         Implement   : Power_Implement;
         Action      : Chaos.Actions.Chaos_Action;
         Target      : Power_Attack_Target;
         Target_Size : Natural := 1;
         Short_Range : Natural := 1;
         Long_Range  : Natural := 1;
         Attack      : Chaos.Expressions.Chaos_Expression;
         Defence     : Chaos.Expressions.Chaos_Expression;
         Hit         : Power_Effect_Vectors.Vector;
         Miss        : Power_Effect_Vectors.Vector;
      end record;

   overriding function Object_Database
     (Object : Chaos_Power_Record)
      return Memor.Root_Database_Type'Class;

   package Power_Vectors is new Ada.Containers.Vectors (Positive, Chaos_Power);

   type Power_Collection is
     new Powered_Interface with
      record
         Powers : Power_Vectors.Vector;
      end record;

end Chaos.Powers;
