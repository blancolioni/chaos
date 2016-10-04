with Chaos.Abilities;
with Chaos.Defences;

with Chaos.Powers.Attacks;

with Chaos.Dice;

with Chaos.UI.Logging;

with Chaos.Actors.Db;

package body Chaos.Actors.Attacks is

   procedure Perform_Attack
     (Attacker      : Chaos.Actors.Chaos_Actor;
      Defender      : Chaos.Actors.Chaos_Actor;
      Attack_Bonus  : Integer;
      Defence       : Integer;
      Damage        : Chaos.Dice.Die_Roll;
      Damage_Bonus  : Integer;
      Damage_Form   : Chaos.Powers.Power_Damage_Type;
      Critical_Roll : Positive := 20);

   --------------------
   -- Perform_Attack --
   --------------------

   procedure Perform_Attack
     (Attacker      : Chaos.Actors.Chaos_Actor;
      Defender      : Chaos.Actors.Chaos_Actor;
      Attack_Bonus  : Integer;
      Defence       : Integer;
      Damage        : Chaos.Dice.Die_Roll;
      Damage_Bonus  : Integer;
      Damage_Form   : Chaos.Powers.Power_Damage_Type;
      Critical_Roll : Positive := 20)
   is
      Roll          : constant Positive := Chaos.Dice.Roll (20);
      Attack        : constant Integer := Roll + Attack_Bonus;
      Success       : constant Boolean := Attack >= Defence;
      Critical_Hit  : constant Boolean := Roll >= Critical_Roll;
      Critical_Miss : constant Boolean := Roll = 1;
      Hit           : constant Boolean :=
                        not Critical_Miss
                            and then (Critical_Hit or else Success);
   begin

      Chaos.UI.Logging.Current_Logger.Log_Attack_Roll
        (Attacker, Roll, Attack_Bonus, Defence,
         Hit, Critical_Hit, Critical_Miss);

      if Hit then

         declare
            Base_Damage   : constant Integer :=
                              Chaos.Dice.Roll (Damage)
                              + (if Critical_Hit
                                 then Chaos.Dice.Roll (Damage) else 0)
                              + Damage_Bonus;
            Actual_Damage : constant Positive := Integer'Max (Base_Damage, 1);
         begin
            Chaos.UI.Logging.Current_Logger.Log_Damage
              (Attacker => Attacker,
               Defender => Defender,
               Damage   => Actual_Damage,
               Resisted => 0);

            if Actual_Damage >= Defender.Creature.Current_Hit_Points then
               Defender.Take_Damage
                 (Damage_Form, Defender.Current_Hit_Points);
               Chaos.UI.Logging.Current_Logger.Log_Death (Defender);
               Defender.Kill;
            else
               Defender.Take_Damage (Damage_Form, Actual_Damage);
            end if;
         end;
      end if;
   end Perform_Attack;

   ------------------
   -- Power_Attack --
   ------------------

   procedure Power_Attack
     (Attacker      : Chaos.Actors.Chaos_Actor;
      Defender      : Chaos.Actors.Chaos_Actor;
      Power         : Chaos.Powers.Chaos_Power)
   is
   begin
--    Chaos.Actors.Db.Update (Attacker.Reference, Use_Standard_Action'Access);

      Chaos.Powers.Attacks.Attack
        (Attacker, Defender, Power);

--        Power.Attack (Attacker, Defender);

--
--        Use_Standard_Action (Attacker);
--        Look_At (Attacker, To);

--        if Resource /= "" then
--           declare
--              Animation : Chaos.Resources.Animations.Animation_Type;
--           begin
--              Chaos.Resources.Animations.Get_Animation_Resource
--                (From         => From,
--                 To           => To,
--                 Name         => Resource,
--                 Orientations =>
--                   Chaos.Powers.Animation_Orientations (Power),
--                 Animation    => Animation);
--
--              On_Attack_Animation
--                (Attacker, Defender, Power, (From, To), Animation);
--           end;
--        else
--           Chaos.Powers.Attack (Attacker, Defender, Power, Extra_Effects);
--        end if;
   end Power_Attack;

   ------------------
   -- Power_Attack --
   ------------------

   procedure Power_Attack
     (Attacker      : Chaos.Actors.Chaos_Actor;
      Target        : Chaos.Locations.Square_Location;
      Power         : Chaos.Powers.Chaos_Power)
   is
   begin
      Db.Update (Attacker.Reference, Use_Standard_Action'Access);
      Power.Attack (Attacker, Target);
--        Look_At (Attacker, Target);
--        Chaos.Powers.Attack (Attacker, Target, Power, Extra_Effects);
   end Power_Attack;

   -------------------
   -- Weapon_Attack --
   -------------------

   procedure Weapon_Attack
     (Attacker : Chaos.Actors.Chaos_Actor;
      Defender : Chaos.Actors.Chaos_Actor;
      Weapon   : Chaos.Items.Weapons.Chaos_Weapon)
   is
      use Chaos.Abilities;
      Str_Bonus : constant Ability_Bonus_Range :=
                    Attacker.Creature.Ability_Bonus (Str);
      Dex_Bonus : constant Ability_Bonus_Range :=
                    Attacker.Creature.Ability_Bonus (Dex);
      Bonus     : constant Ability_Bonus_Range :=
                    (if Weapon.Has_Property (Chaos.Items.Weapons.Finesse)
                     then Ability_Bonus_Range'Max (Str_Bonus, Dex_Bonus)
                     else Str_Bonus);
      Proficiency : constant Natural :=
                      Attacker.Creature.Level_Bonus;
      AC          : constant Chaos.Defences.Defence_Score_Range :=
                      Defender.Creature.Defence_Score
                        (Chaos.Defences.AC);
   begin

      Chaos.UI.Logging.Current_Logger.Log_Attack (Attacker, Defender, null);

      Perform_Attack
        (Attacker      => Attacker,
         Defender      => Defender,
         Attack_Bonus  => Integer (Bonus) + Proficiency,
         Defence       => Integer (AC),
         Damage_Form   => Chaos.Powers.Normal,
         Damage        => Weapon.Damage,
         Damage_Bonus  => Integer (Bonus),
         Critical_Roll => 20);

   end Weapon_Attack;

end Chaos.Actors.Attacks;
