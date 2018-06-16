

with Chaos.Actors;
with Chaos.Dice;
with Chaos.Localisation;

with Chaos.UI;

package body Chaos.Powers.Attacks is

   procedure Apply_Power_Effects
     (Actor       : Chaos.Actors.Chaos_Actor;
      Defender    : Chaos.Actors.Chaos_Actor;
      Power       : Chaos.Powers.Chaos_Power;
      Effects     : Lith.Objects.Object;
      Roll        : Positive;
      Hit         : Boolean;
      Critical    : Boolean);

   -------------------------
   -- Apply_Power_Effects --
   -------------------------

   procedure Apply_Power_Effects
     (Actor       : Chaos.Actors.Chaos_Actor;
      Defender    : Chaos.Actors.Chaos_Actor;
      Power       : Chaos.Powers.Chaos_Power;
      Effects     : Lith.Objects.Object;
      Roll        : Positive;
      Hit         : Boolean;
      Critical    : Boolean)
   is
      use Lith.Objects, Lith.Objects.Symbols;
      use Chaos.Expressions;
      use Chaos.Localisation;

      Normal_Damage   : Integer := 0;
      Damage_Type     : Power_Damage_Type := Normal;

   begin

      Store.New_Evaluation_Environment;
      Store.Add_Binding (Get_Symbol ("actor"), Actor.To_Expression);
      Store.Add_Binding (Get_Symbol ("defender"), Defender.To_Expression);
      Store.Add_Binding (Get_Symbol ("power"), Power.To_Expression);
      Store.Add_Binding
        (Get_Symbol ("roll"), Lith.Objects.To_Object (Roll));

      Store.Add_Binding (Get_Symbol ("hit"),
                            Lith.Objects.To_Object (Hit));
      Store.Add_Binding (Get_Symbol ("critical"),
                            Lith.Objects.To_Object (Critical));

      if Power.Implement = Weapon then
         Store.Add_Binding
           (Get_Symbol ("weapon"),
            Chaos.Dice.To_Expression
              (Actor.Creature.Active_Weapon.Damage));
      end if;

      declare
         It : Object := Effects;
      begin
         while It /= Nil loop
            Store.Push (It, Stack => Secondary);
            declare
               Effect : constant Object :=
                          Store.Evaluate_With_Environment
                            (Store.Car (Store.Top (1, Secondary)));
            begin
               if Is_Power_Damage_Type (Effect) then
                  Damage_Type := Get_Power_Damage_Type (Effect);
               else
                  declare
                     D : constant Positive :=
                           Integer'Max (To_Integer (Effect), 1);
                  begin
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
                  end;
               end if;
            end;
            It := Store.Pop (Secondary);
            Store.Push (Store.Cdr (It), Secondary);
         end loop;
      end;

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

      Store : constant access Lith.Objects.Object_Store'Class :=
                Chaos.Expressions.Store;

      Attack_Value  : Integer;
      Defence_Value : Integer;
      Roll          : constant Positive :=
                        Chaos.Dice.Roll
                          (1, 20, 0);
   begin

      Attack_Value :=
        Lith.Objects.To_Integer
          (Store.Evaluate (Power.Attack));

      Defence_Value :=
        Lith.Objects.To_Integer
          (Store.Evaluate (Power.Defence));

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
           (Actor, Defender, Power,
            (if Hit then Power.Hit_Effects else Power.Miss_Effects),
            Roll, Hit, Critical);
         Apply_Power_Effects
           (Actor, Defender, Power,
            Power.Effects,
            Roll, Hit, Critical);
      end;

   end Attack;

end Chaos.Powers.Attacks;
