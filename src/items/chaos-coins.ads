package Chaos.Coins is

   type Unit_Type is (Copper, Silver, Gold, Platinum);

   type Coin_Count is new Natural;
   type Coin_Value is new Natural;

   function "*" (Left  : Coin_Count;
                 Right : Coin_Value)
                 return Coin_Value;

   type Coins_Type is private;

   function "+" (Left, Right : Coins_Type) return Coins_Type;
   function "-" (Left, Right : Coins_Type) return Coins_Type;

   function Weight (Coins : Coins_Type) return Natural;

   function Value (Coins : Coins_Type;
                   Unit  : Unit_Type := Gold)
                   return Coin_Value;

   function Quantity (Coins : Coins_Type;
                      Unit  : Unit_Type)
                      return Coin_Count;

   function Total_Quantity
     (Coins : Coins_Type)
      return Coin_Count;

   function Coins (Amount : Coin_Count;
                   Unit   : Unit_Type)
                   return Coins_Type;

   function CP (Amount : Coin_Count) return Coins_Type
   is (Coins (Amount, Copper));

   function SP (Amount : Coin_Count) return Coins_Type
   is (Coins (Amount, Silver));

   function GP (Amount : Coin_Count) return Coins_Type
   is (Coins (Amount, Gold));

   function PP (Amount : Coin_Count) return Coins_Type
   is (Coins (Amount, Platinum));

   procedure Add_Coins
     (To     : in out Coins_Type;
      Amount : Coin_Count;
      Unit   : Unit_Type);

   procedure Remove_Coins
     (To     : in out Coins_Type;
      Amount : Coin_Count;
      Unit   : Unit_Type);

   procedure Remove_Value
     (To     : in out Coins_Type;
      Amount : Coin_Value;
      Unit   : Unit_Type);

   function To_Coins (Image : String) return Coins_Type;

private

   type Coins_Type is array (Unit_Type) of Coin_Count;

end Chaos.Coins;
