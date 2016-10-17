with WL.Binary_IO;

with Chaos.Localisation;
with Chaos.Logging;

with Chaos.Resources.Cre;
with Chaos.Resources.Manager;

with Chaos.Expressions.Import;
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

      procedure Create (Creature : in out Chaos_Creature_Record'Class)
      is null;

      procedure Configure (Creature : in out Chaos_Creature_Record'Class);

      ---------------
      -- Configure --
      ---------------

      procedure Configure (Creature : in out Chaos_Creature_Record'Class) is
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

         Chaos.Expressions.Import.Import_Scripts (Cre.Scripts);
         Creature.Script := Chaos.Expressions.Store.Pop;

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

         if Chaos.Resources.Has_Resource (Cre.Dialog_Ref) then
            begin
               Creature.Dialog :=
                 Chaos.Dialog.Import.Import_Dialog
                   (Resources.To_String (Cre.Dialog_Ref));
            exception
               when others =>
                  Chaos.Logging.Log
                    ("IMPORT",
                     "cannot import dialog "
                     & Resources.To_String (Cre.Dialog_Ref)
                     & " for creature " & Name);
            end;
         end if;

      end Configure;

      Creature : constant Chaos_Creature :=
                   Db.Create (Create'Access);
   begin
      Chaos.Logging.Log
        ("CREATURE", Name);
      Creature.Save_Object;
      Creature.Update (Configure'Access);
      Creature.Define_Object;
      return Creature;
   end Import_Creature;

end Chaos.Creatures.Import;
