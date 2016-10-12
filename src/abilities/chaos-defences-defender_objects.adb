package body Chaos.Defences.Defender_Objects is

   ----------------------------
   -- Add_Defence_Properties --
   ----------------------------

   procedure Add_Defence_Properties
     (Object : Defender_Object'Class)
   is
   begin
      Object.Add_Property ("fort", Get_Fort);
      Object.Add_Property ("refl", Get_Refl);
      Object.Add_Property ("will", Get_Will);
      Object.Add_Property ("ac", Get_AC);
   end Add_Defence_Properties;

end Chaos.Defences.Defender_Objects;
