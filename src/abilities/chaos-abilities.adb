package body Chaos.Abilities is

   -----------
   -- Apply --
   -----------

   procedure Apply
     (Score  : in out Ability_Score_Range;
      Change : Ability_Score_Change)
   is
   begin
      Score := Ability_Score_Range (Integer (Score) + Integer (Change));
   end Apply;

end Chaos.Abilities;
