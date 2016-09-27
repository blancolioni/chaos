package Chaos.Defences is

   type Defence is (Fort, Refl, Will, AC);

   type Defence_Score_Range is range 1 .. 30;

   type Defence_Score_Change is range -10 .. 10;

   type Defence_Scores is
     array (Defence) of Defence_Score_Range;

   type Defence_Score_Changes is
     array (Defence) of Defence_Score_Change;

   type Defence_Interface is limited interface;

   function Defence_Score
     (Defender : Defence_Interface;
      Item     : Defence)
      return Defence_Score_Range
      is abstract;

end Chaos.Defences;
