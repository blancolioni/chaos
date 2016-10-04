with Chaos.Actors;
with Chaos.Dice;

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
                      Chaos.Expressions.Prepend_Environmenet
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

      Attack_Value :=
        Chaos.Expressions.Numbers.To_Integer
          (Chaos.Expressions.Evaluate (Power.Attack, Environment));

      Defence_Value :=
        Chaos.Expressions.Numbers.To_Integer
          (Chaos.Expressions.Evaluate
             (Power.Defence, Defender.Local_Environment));

      declare
         Hit : constant Boolean := Attack_Value + Roll >= Defence_Value;
         Critical : constant Boolean := Roll = 20
           and then Attack_Value + 20 > Defence_Value;
      begin
         Chaos.UI.Current_UI.Display_Text
           (Actor.Short_Name & " attacls " & Defender.Short_Name
            & " using " & Power.Display_Name
            & ": attack" & Attack_Value'Img & " vs defence"
            & Defence_Value'Img & "; roll =" & Roll'Img
            & ": "
            & (if Critical
              then "critical hit"
              elsif Hit
              then "hit"
              else "miss"));
      end;

   end Attack;

end Chaos.Powers.Attacks;
