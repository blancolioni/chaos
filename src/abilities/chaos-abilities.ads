with Chaos.Expressions;

package Chaos.Abilities is

   type Ability is (Str, Con, Dex, Int, Wis, Cha);

   type Ability_Score_Range is range 1 .. 30;

   type Ability_Score_Change is range -5 .. 5;

   type Ability_Bonus_Range is range -5 .. 10;

   type Ability_Scores is array (Ability) of Ability_Score_Range;

   type Ability_Score_Changes is
     array (Ability) of Ability_Score_Change;

   type Ability_Interface is limited interface;

   function Ability_Score
     (Able : Ability_Interface;
      Item : Ability)
      return Ability_Score_Range
      is abstract;

   function Ability_Bonus
     (Able : Ability_Interface'Class;
      Item : Ability)
      return Ability_Bonus_Range
   is (Ability_Bonus_Range (Integer (Able.Ability_Score (Item)) / 2 - 5));

   procedure Insert_Abilities
     (Able : Ability_Interface'Class;
      Env  : in out Chaos.Expressions.Chaos_Environment);

end Chaos.Abilities;
