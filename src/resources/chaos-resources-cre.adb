with Chaos.Localisation;
with Chaos.Logging;

package body Chaos.Resources.Cre is

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Cre : in out Cre_Resource)
   is
   begin
      Cre.Set_Offset (8);
      Cre.Get (Cre.Long_Name);
      Cre.Get (Cre.Short_Name);
      Cre.Get (Cre.Flags);
      Cre.Get (Cre.XP_Value);
      Cre.Get (Cre.Power_Level);
      Cre.Get (Cre.Gold);
      Cre.Get (Cre.Status_Flags);
      Cre.Get (Cre.Current_Hit_Points);
      Cre.Get (Cre.Maximum_Hit_Points);
      Cre.Get (Cre.Animation_Id);
      for I in Cre.Colours'Range loop
         Cre.Get (Cre.Colours (I));
      end loop;

      Cre.Get (Cre.EFF_Version);
      Cre.Get (Cre.Small_Portrait);
      Cre.Get (Cre.Large_Portrait);
      Cre.Get (Cre.Reputation);
      Cre.Get (Cre.Base_Hide_In_Shadows);
      for I in Cre.Armor_Class'Range loop
         Cre.Get (Cre.Armor_Class (I));
      end loop;
      Cre.Get (Cre.Thac0);
      Cre.Get (Cre.Number_Of_Attacks);
      for I in Cre.Saving_Throws'Range loop
         Cre.Get (Cre.Saving_Throws (I));
      end loop;
      for I in Cre.Resistance'Range loop
         Cre.Get (Cre.Resistance (I));
      end loop;
      for I in Cre.Thieving'Range loop
         Cre.Get (Cre.Thieving (I));
      end loop;
      Cre.Get (Cre.Fatigue);
      Cre.Get (Cre.Intoxication);
      Cre.Get (Cre.Luck);
      for I in Cre.Proficiencies'Range loop
         Cre.Get (Cre.Proficiencies (I));
      end loop;
      Cre.Get (Cre.Turn_Undead_Level);
      Cre.Get (Cre.Tracking_Skill);
      for I in Cre.Tracking_Target'Range loop
         declare
            X : Word_8;
         begin
            Cre.Get (X);
            Cre.Tracking_Target (I) := Character'Val (X);
         end;
      end loop;
      for I in Cre.String_Refs'Range loop
         Cre.Get (Cre.String_Refs (I));
      end loop;
      Cre.Get (Cre.Highest_Level_1);
      Cre.Get (Cre.Highest_Level_2);
      Cre.Get (Cre.Highest_Level_3);
      Cre.Get (Cre.Gender);
      for I in Cre.Abilities'Range loop
         Cre.Get (Cre.Abilities (I));
      end loop;
      Cre.Get (Cre.Morale);
      Cre.Get (Cre.Morale_Break);
      Cre.Get (Cre.Racial_Enemy);
      Cre.Get (Cre.Morale_Recovery_Time);
      Cre.Get (Cre.Kit);

      Cre.Scripts := (others => (others => Character'Val (0)));

      Cre.Get (Cre.Scripts (Override_Script));
      Cre.Get (Cre.Scripts (Class_Script));
      Cre.Get (Cre.Scripts (Race_Script));
      Cre.Get (Cre.Scripts (General_Script));
      Cre.Get (Cre.Scripts (Default_Script));

      for I in Cre.Ids'Range loop
         Cre.Get (Cre.Ids (I));
      end loop;
      Cre.Get (Cre.Global_Actor);
      Cre.Get (Cre.Local_Actor);
      for I in Cre.Death_Variable'Range loop
         declare
            X : Word_8;
         begin
            Cre.Get (X);
            Cre.Death_Variable (I) := Character'Val (X);
         end;
      end loop;
      Cre.Get (Cre.Known_Spells_Offset);
      Cre.Get (Cre.Known_Spells_Count);
      Cre.Get (Cre.Spell_Memorisation_Offset);
      Cre.Get (Cre.Spell_Memorisation_Count);
      Cre.Get (Cre.Memorised_Spells_Offset);
      Cre.Get (Cre.Memorised_Spells_Count);
      Cre.Get (Cre.Entity_Slot_Offset);
      Cre.Get (Cre.Entity_Offset);
      Cre.Get (Cre.Entity_Count);
      Cre.Get (Cre.Effect_Offset);
      Cre.Get (Cre.Effect_Count);
      Cre.Get (Cre.Dialog_Ref);

      if False then
         Chaos.Logging.Log
           ("CRE",
            Chaos.Localisation.Indexed_Text
              (Chaos.Localisation.Local_Text_Index
                   (Natural (Cre.Short_Name)))
            & ": anim-id = "
            & Word_32'Image (Cre.Animation_Id));
      end if;

   end Load;

end Chaos.Resources.Cre;
