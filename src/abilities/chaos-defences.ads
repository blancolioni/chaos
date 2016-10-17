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
      Entity     : Defence)
      return Defence_Score_Range
      is abstract;

--     procedure Insert_Defences
--       (Defender : Defence_Interface'Class;
--        Env      : in out Chaos.Expressions.Chaos_Environment);

end Chaos.Defences;
