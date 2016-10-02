with WL.Binary_IO;

with Chaos.Localisation;
with Chaos.Logging;

with Chaos.Resources.Cre;
with Chaos.Resources.Manager;

with Chaos.Classes.Import;
with Chaos.Races.Import;

with Chaos.Dialog.Import;

with Chaos.Creatures.Db;

package body Chaos.Creatures.Import is

   ---------------------
   -- Import_Creature --
   ---------------------

   function Import_Creature
     (Name : String)
      return Chaos_Creature
   is
      Cre : Chaos.Resources.Cre.Cre_Resource'Class renames
              Chaos.Resources.Cre.Cre_Resource'Class
                (Chaos.Resources.Manager.Load_Resource
                   (Reference => Chaos.Resources.To_Reference (Name),
                    Res_Type  => Chaos.Resources.Creature_Resource).all);

      procedure Create (Creature : in out Chaos_Creature_Record'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Creature : in out Chaos_Creature_Record'Class) is
         use type WL.Binary_IO.Word_32;
         use Chaos.Resources.Cre;
      begin
         Creature.Initialize (Name);
         Creature.Add_Power (Chaos.Powers.Get ("basic-melee-attack"));
         Creature.Short_Name :=
           Ada.Strings.Unbounded.To_Unbounded_String
             (Chaos.Localisation.Indexed_Text
                (Chaos.Localisation.Local_Text_Index
                   (Cre.Short_Name)));
         Creature.Long_Name :=
           (if Cre.Long_Name < 2 ** 20
            then Ada.Strings.Unbounded.To_Unbounded_String
              (Chaos.Localisation.Indexed_Text
                   (Chaos.Localisation.Local_Text_Index
                        (Cre.Long_Name)))
            else Creature.Short_Name);

         Creature.Race :=
           Chaos.Races.Import.Import_Race (Natural (Cre.Ids (Race)));
         Creature.Class :=
           Chaos.Classes.Import.Import_Class (Natural (Cre.Ids (Class)));

         Creature.Abilities (Chaos.Abilities.Str) :=
           Chaos.Abilities.Ability_Score_Range (Cre.Abilities (Str));
         Creature.Abilities (Chaos.Abilities.Con) :=
           Chaos.Abilities.Ability_Score_Range (Cre.Abilities (Con));
         Creature.Abilities (Chaos.Abilities.Dex) :=
           Chaos.Abilities.Ability_Score_Range (Cre.Abilities (Dex));
         Creature.Abilities (Chaos.Abilities.Int) :=
           Chaos.Abilities.Ability_Score_Range (Cre.Abilities (Int));
         Creature.Abilities (Chaos.Abilities.Wis) :=
           Chaos.Abilities.Ability_Score_Range (Cre.Abilities (Wis));
         Creature.Abilities (Chaos.Abilities.Cha) :=
           Chaos.Abilities.Ability_Score_Range (Cre.Abilities (Cha));

         Creature.Level :=
           Chaos.Levels.Chaos_Level (Cre.Power_Level);
         Creature.HP := Creature.Max_Hit_Points;
         Creature.Animation_Id := Natural (Cre.Animation_Id);

         Creature.Color_Map (Metal) :=
           Natural (Cre.Colours (Metal));
         Creature.Color_Map (Minor) :=
           Natural (Cre.Colours (Minor));
         Creature.Color_Map (Major) :=
           Natural (Cre.Colours (Major));
         Creature.Color_Map (Skin) :=
           Natural (Cre.Colours (Skin));
         Creature.Color_Map (Leather) :=
           Natural (Cre.Colours (Leather));
         Creature.Color_Map (Armour) :=
           Natural (Cre.Colours (Armour));
         Creature.Color_Map (Hair) :=
           Natural (Cre.Colours (Hair));

         Creature.Team := Chaos.Teams.Get ("neutral");

         declare
            Dlg : constant String :=
                    Chaos.Resources.To_String
                      (Cre.Dialog_Ref);
         begin
            if Dlg'Length > 0
              and then Dlg /= "NONE"
            then
               begin
                  Creature.Dialog :=
                    Chaos.Dialog.Import.Import_Dialog (Dlg);
               exception
                  when others =>
                     Chaos.Logging.Log
                       ("IMPORT",
                        "cannot import dialog " & Dlg
                        & " for creature " & Name);
               end;
            end if;
         end;

      end Create;

   begin
      return Db.Create (Create'Access);
   end Import_Creature;

end Chaos.Creatures.Import;
