private with Memor;

private with Chaos.Expressions.Enumerated;

with Chaos.Abilities;
with Chaos.Defences;
with Chaos.Powers;

with Chaos.Animated;

with Chaos.Objects;

package Chaos.Classes is

   type Class_Role is (Controller, Defender, Leader, Striker);

   type Class_Power_Source is (Arcane, Divine, Martial);

   type Chaos_Class_Record is
     new Chaos.Objects.Root_Localised_Object_Record
     and Chaos.Animated.Animated_Interface
     and Chaos.Powers.Powered_Interface
   with private;

   function Base_Hit_Points
     (Class : Chaos_Class_Record'Class)
      return Natural;

   function Level_Hit_Points
     (Class : Chaos_Class_Record'Class)
      return Natural;

   overriding function Power_Count
     (Class : Chaos_Class_Record)
      return Natural;

   overriding function Get_Power
     (Class : Chaos_Class_Record;
      Index      : Positive)
      return Chaos.Powers.Chaos_Power;

   type Chaos_Class is access constant Chaos_Class_Record'Class;

   function Get (Identifier : String) return Chaos_Class;

   procedure Scan
     (Process : not null access
        procedure (Class : Chaos_Class));

   type Chaos_Class_Interface is limited interface;

   function Class (Item : Chaos_Class_Interface) return Chaos_Class
                   is abstract;

   function Class_Defence_Bonus
     (Item    : Chaos_Class_Interface'Class;
      Defence : Chaos.Defences.Defence)
      return Chaos.Defences.Defence_Score_Change;

   function Base_Hit_Points
     (Item : Chaos_Class_Interface'Class)
      return Natural;

   function Level_Hit_Points
     (Item : Chaos_Class_Interface'Class)
      return Natural;

private

   type Key_Ability_Array is array (1 .. 6) of Chaos.Abilities.Ability;

   type Chaos_Class_Record is
     new Chaos.Objects.Root_Localised_Object_Record
     and Chaos.Animated.Animated_Interface
     and Chaos.Powers.Powered_Interface with
      record
         Animation_Code   : Character;
         Key_Abilities    : Key_Ability_Array;
         Defences         : Chaos.Defences.Defence_Score_Changes :=
                              (others => 0);
         Role             : Class_Role;
         Power_Source     : Class_Power_Source;
         Base_Hit_Points  : Natural;
         Level_Hit_Points : Natural;
         Healing_Surges   : Natural;
         Powers           : Chaos.Powers.Power_Collection;
      end record;

   overriding function Object_Database
     (Object : Chaos_Class_Record)
      return Memor.Root_Database_Type'Class;

   overriding procedure Add_Power
     (Class      : in out Chaos_Class_Record;
      Power      : Chaos.Powers.Chaos_Power);

   overriding function Animation_Code
     (Class : Chaos_Class_Record)
      return Character
   is (Class.Animation_Code);

   package Role_Expressions is
     new Chaos.Expressions.Enumerated (Class_Role);

   package Power_Source_Expressions is
     new Chaos.Expressions.Enumerated (Class_Power_Source);

end Chaos.Classes;
