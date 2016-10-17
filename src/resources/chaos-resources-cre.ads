package Chaos.Resources.Cre is

   use WL.Binary_IO;

   type Colour_Part is (Metal, Minor, Major, Skin, Leather, Armour, Hair);

   type Colour_Array is array (Colour_Part) of Word_8;

   type Armour_Classes is (Natural_AC, Effective_AC,
                           Crushing_Mod, Missile_Mod,
                           Piercing_Mod, Slashing_Mod);

   type Armour_Class_Array is array (Armour_Classes) of Word_16;

   type Saving_Throws is (Death, Wants, Polymorph, Breath, Fire);

   type Saving_Throw_Array is array (Saving_Throws) of Word_8;

   type Resistances is (Fire, Cold, Electricity, Acid,
                        Magic, Magic_Fire, Magic_Cold,
                        Slashing, Crushing, Piercing, Missile);

   type Resistance_Array is array (Resistances) of Word_8;

   type Thieving is (Detect_Illusion, Set_Traps, Lore, Pick_Locks,
                     Steal, Find_And_Remove_Traps, Pick_Pockets);

   type Thieving_Array is array (Thieving) of Word_8;

   type Proficiency is (Large_Swords, Small_Swords, Bows, Spears,
                        Blunt, Spiked, Axe, Missile,
                        Unknown_1, Unknown_2, Unknown_3, Unknown_4,
                        Unknown_5, Unknown_6, Unknown_7, Unknown_8,
                        Unknown_9, Unknown_10, Unknown_11, Unknown_12);

   type Proficiency_Array is array (Proficiency) of Word_8;

   type String_Ref_List is array (1 .. 100) of String_Reference;

   type Ability is (Str, Str_Bonus, Int, Wis, Dex, Con, Cha);

   type Ability_Array is array (Ability) of Word_8;

   type Ids_Reference is (Enemy_Ally, General, Race, Class, Specific, Gender,
                          Object_1, Object_2, Object_3, Object_4, Object_5,
                          Alignment);

   type Id_Reference_Array is array (Ids_Reference) of Word_8;

   type Object_Ids is array (1 .. 5) of Word_8;

   type Cre_Resource is
     new Chaos_Resource with
      record
         Long_Name                 : String_Reference;
         Short_Name                : String_Reference;
         Flags                     : Word_32;
         XP_Value                  : Word_32;
         Power_Level               : Word_32;
         Gold                      : Word_32;
         Status_Flags              : Word_32;
         Current_Hit_Points        : Word_16;
         Maximum_Hit_Points        : Word_16;
         Animation_Id              : Word_32;
         Colours                   : Colour_Array;
         EFF_Version               : Word_8;
         Small_Portrait            : Resource_Reference;
         Large_Portrait            : Resource_Reference;
         Reputation                : Integer_8;
         Base_Hide_In_Shadows      : Word_8;
         Armor_Class               : Armour_Class_Array;
         Thac0                     : Word_8;
         Number_Of_Attacks         : Word_8;
         Saving_Throws             : Saving_Throw_Array;
         Resistance                : Resistance_Array;
         Thieving                  : Thieving_Array;
         Fatigue                   : Word_8;
         Intoxication              : Word_8;
         Luck                      : Word_8;
         Proficiencies             : Proficiency_Array;
         Turn_Undead_Level         : Word_8;
         Tracking_Skill            : Word_8;
         Tracking_Target           : String (1 .. 32);
         String_Refs               : String_Ref_List;
         Highest_Level_1           : Word_8;
         Highest_Level_2           : Word_8;
         Highest_Level_3           : Word_8;
         Gender                    : Word_8;
         Abilities                 : Ability_Array;
         Morale                    : Word_8;
         Morale_Break              : Word_8;
         Racial_Enemy              : Word_8;
         Morale_Recovery_Time      : Word_16;
         Kit                       : Word_32;
         Scripts                   : Script_Array;
         Ids                       : Id_Reference_Array;
         Global_Actor              : Word_16;
         Local_Actor               : Word_16;
         Death_Variable            : String (1 .. 32);
         Known_Spells_Offset       : Word_32;
         Known_Spells_Count        : Word_32;
         Spell_Memorisation_Offset : Word_32;
         Spell_Memorisation_Count  : Word_32;
         Memorised_Spells_Offset   : Word_32;
         Memorised_Spells_Count    : Word_32;
         Entity_Slot_Offset          : Word_32;
         Entity_Offset               : Word_32;
         Entity_Count                : Word_32;
         Effect_Offset             : Word_32;
         Effect_Count              : Word_32;
         Dialog_Ref                : Resource_Reference;
      end record;

   overriding function Signature
     (Cre : Cre_Resource)
      return String
   is ("CRE ");

   overriding procedure Load
     (Cre : in out Cre_Resource);

end Chaos.Resources.Cre;
