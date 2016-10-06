with Chaos.Actors;
with Chaos.Dice;
with Chaos.Localisation;

with Chaos.Expressions.Environments;
with Chaos.Expressions.Numbers;
with Chaos.Expressions.Vectors;

with Chaos.UI;

package body Chaos.Powers.Attacks is

   procedure Apply_Power_Effects
     (Actor       : Chaos.Actors.Chaos_Actor;
      Defender    : Chaos.Actors.Chaos_Actor;
      Power       : Chaos.Powers.Chaos_Power;
      Environment : Chaos.Expressions.Chaos_Environment;
      Effects     : Power_Effect_Vectors.Vector;
      Hit         : Boolean;
      Critical    : Boolean);

   -------------------------
   -- Apply_Power_Effects --
   -------------------------

   procedure Apply_Power_Effects
     (Actor       : Chaos.Actors.Chaos_Actor;
      Defender    : Chaos.Actors.Chaos_Actor;
      Power       : Chaos.Powers.Chaos_Power;
      Environment : Chaos.Expressions.Chaos_Environment;
      Effects     : Power_Effect_Vectors.Vector;
      Hit         : Boolean;
      Critical    : Boolean)
   is
      pragma Unreferenced (Hit);

      use Chaos.Expressions;
      use Chaos.Expressions.Numbers;
      use Chaos.Expressions.Vectors;
      use Chaos.Localisation;

      Hit_Environment : Chaos_Environment :=
                          New_Environment
                            (Environment);
      Normal_Damage   : Integer := 0;
      Damage_Type     : Power_Damage_Type := Normal;
   begin
      if Critical then
         Chaos.Expressions.Insert
           (Hit_Environment, "maximum-roll",
            Chaos.Expressions.Always);
      end if;

      if Power.Implement = Weapon then
         Chaos.Expressions.Insert
           (Hit_Environment, "weapon",
            Chaos.Dice.To_Expression
              (Actor.Creature.Active_Weapon.Damage));
      end if;

      for Effect of Effects loop
         for I in 1 .. Length (Effect) loop
            declare
               E : constant Chaos_Expression :=
                     Evaluate (Get (Effect, I), Hit_Environment);
               D : Integer;
            begin
               if Is_Power_Damage_Type (E) then
                  Damage_Type := Get_Power_Damage_Type (E);
               else
                  D := Integer'Max (To_Integer (E), 1);
                  if Damage_Type /= Normal then
                     Chaos.UI.Current_UI.Put_Line
                       (Local_Text
                          ("takes-damage-type",
                           Defender.Short_Name, D'Img,
                           Local_Text
                             (Power_Damage_Type'Image
                                  (Damage_Type))));
                  else
                     Normal_Damage := Normal_Damage + D;
                  end if;
               end if;
            end;
         end loop;
      end loop;

      if Normal_Damage > 0 then
         Chaos.UI.Current_UI.Put_Line
           (Local_Text
              ("takes-damage",
               Defender.Short_Name, Normal_Damage'Img));
      end if;
   end Apply_Power_Effects;

   ------------
   -- Attack --
   ------------

   procedure Attack
     (Actor     : Chaos.Actors.Chaos_Actor;
      Defender  : Chaos.Actors.Chaos_Actor;
      Power     : Chaos.Powers.Chaos_Power)
   is
      use Chaos.Expressions;

      Environment : Chaos.Expressions.Chaos_Environment :=
                      Chaos.Expressions.Prepend_Environment
                        (Actor.Local_Environment,
                         Chaos.Expressions.Environments.Standard_Environment);

      Attack_Value  : Integer;
      Defence_Value : Integer;
      Roll          : constant Positive :=
                        Chaos.Dice.Roll
                          (1, 20, 0);
   begin
      Chaos.Expressions.Insert
        (Environment, "actor", Actor.To_Expression);
      Chaos.Expressions.Insert
        (Environment, "defender", Defender.To_Expression);
      Chaos.Expressions.Insert
        (Environment, "power", Power.To_Expression);

      declare
         Attack_Environment : constant Chaos.Expressions.Chaos_Environment :=
                                Chaos.Expressions.Prepend_Environment
                                  (Actor.Creature.Ability_Modifiers,
                                   Environment);
      begin
         Attack_Value :=
           Chaos.Expressions.Numbers.To_Integer
             (Chaos.Expressions.Evaluate (Power.Attack, Attack_Environment));
      end;

      Defence_Value :=
        Chaos.Expressions.Numbers.To_Integer
          (Chaos.Expressions.Evaluate
             (Power.Defence, Defender.Local_Environment));

      declare
         use Chaos.Localisation;
         Hit : constant Boolean := Attack_Value + Roll >= Defence_Value;
         Critical : constant Boolean := Roll = 20
           and then Attack_Value + 20 > Defence_Value;
      begin
         Chaos.UI.Current_UI.Put_Line
           (Local_Text
              ("x-attacks-y-with-z",
               Actor.Short_Name,
               Defender.Short_Name,
               Power.Display_Name)
            & ": "
            & Local_Text
              ("attack-roll",
               Attack_Value'Img,
               Defence_Value'Img,
               Roll'Img)
              & ": "
            & Local_Text
              ((if Critical
               then "attack-critical"
               elsif Hit
               then "attack-hit"
               else "attack-miss")));

         Apply_Power_Effects
           (Actor, Defender, Power, Environment,
            (if Hit then Power.Hit else Power.Miss),
            Hit, Critical);

      end;

   end Attack;

end Chaos.Powers.Attacks;
