package body Chaos.Abilities.Able_Objects is

   ----------------------------
   -- Add_Ability_Properties --
   ----------------------------

   procedure Add_Ability_Properties
     (Object : Able_Object'Class)
   is
   begin
      Object.Add_Property ("str", Get_Str);
      Object.Add_Property ("con", Get_Con);
      Object.Add_Property ("dex", Get_Dex);
      Object.Add_Property ("int", Get_Int);
      Object.Add_Property ("wis", Get_Wis);
      Object.Add_Property ("cha", Get_Cha);
   end Add_Ability_Properties;

end Chaos.Abilities.Able_Objects;
