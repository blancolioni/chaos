with Chaos.Actors;
with Chaos.Dice;
with Chaos.Localisation;

with Chaos.Expressions.Environments;
with Chaos.Expressions.Numbers;

with Chaos.UI;

package body Chaos.Powers.Attacks is

   ------------
   -- Attack --
   ------------

   procedure Attack
     (Actor     : Chaos.Actors.Chaos_Actor;
      Defender  : Chaos.Actors.Chaos_Actor;
      Power     : Chaos.Powers.Chaos_Power)
   is
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
         Chaos.UI.Current_UI.Display_Text
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
      end;

   end Attack;

end Chaos.Powers.Attacks;
