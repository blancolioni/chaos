with Ada.Characters.Handling;

package body Chaos.Coins is

   Copper_Value : constant array (Unit_Type) of Coin_Value :=
                    (Copper   => 1,
                     Silver   => 10,
                     Gold     => 100,
                     Platinum => 10_000);

   ---------
   -- "*" --
   ---------

   function "*" (Left  : Coin_Count;
                 Right : Coin_Value)
                 return Coin_Value
   is
   begin
      return Coin_Value (Left) * Right;
   end "*";

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Coins_Type) return Coins_Type is
   begin
      return Total : Coins_Type do
         for Unit in Unit_Type loop
            Total (Unit) := Left (Unit) + Right (Unit);
         end loop;
      end return;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left, Right : Coins_Type) return Coins_Type is
   begin
      return Total : Coins_Type do
         for Unit in Unit_Type loop
            pragma Assert (Left (Unit) >= Right (Unit));
            Total (Unit) := Left (Unit) - Right (Unit);
         end loop;
      end return;
   end "-";

   ---------------
   -- Add_Coins --
   ---------------

   procedure Add_Coins
     (To     : in out Coins_Type;
      Amount : Coin_Count;
      Unit   : Unit_Type)
   is
   begin
      To (Unit) := To (Unit) + Amount;
   end Add_Coins;

   -----------
   -- Coins --
   -----------

   function Coins
     (Amount : Coin_Count;
      Unit   : Unit_Type)
      return Coins_Type
   is
   begin
      return Result : Coins_Type := (others => 0) do
         Result (Unit) := Amount;
      end return;
   end Coins;

   --------------
   -- Quantity --
   --------------

   function Quantity
     (Coins : Coins_Type;
      Unit  : Unit_Type)
      return Coin_Count
   is
   begin
      return Coins (Unit);
   end Quantity;

   ------------------
   -- Remove_Coins --
   ------------------

   procedure Remove_Coins
     (To     : in out Coins_Type;
      Amount : Coin_Count;
      Unit   : Unit_Type)
   is
   begin
      pragma Assert (To (Unit) >= Amount);
      To (Unit) := To (Unit) - Amount;
   end Remove_Coins;

   ------------------
   -- Remove_Value --
   ------------------

   procedure Remove_Value
     (To     : in out Coins_Type;
      Amount : Coin_Value;
      Unit   : Unit_Type)
   is
      Value_CP : Coin_Value :=
                   Amount * Copper_Value (Unit);
   begin
      pragma Assert (Value_CP <= Value (To, Copper));
      for Unit in reverse Unit_Type loop
         if To (Unit) * Copper_Value (Unit) >= Value_CP then
            To (Unit) := To (Unit) -
              Coin_Count (Value_CP / Copper_Value (Unit));
            Value_CP := Value_CP mod Copper_Value (Unit);
         end if;
      end loop;

      if Value_CP > 0 then
         declare
            New_CP_Value : Coin_Value :=
                             Value (To, Copper) - Value_CP;
         begin
            for Unit in reverse To'Range loop
               To (Unit) := Coin_Count (New_CP_Value / Copper_Value (Unit));
               New_CP_Value := New_CP_Value mod Copper_Value (Unit);
            end loop;
         end;
      end if;
   end Remove_Value;

   --------------
   -- To_Coins --
   --------------

   function To_Coins (Image : String) return Coins_Type is
      use Ada.Characters.Handling;
      Index : Positive := Image'First;
   begin
      while Index <= Image'Last
        and then Is_Digit (Image (Index))
      loop
         Index := Index + 1;
      end loop;

      if Index = Image'First then
         return (others => 0);
      else
         declare
            Count : constant Coin_Count :=
                      Coin_Count'Value (Image (Image'First .. Index - 1));
            Unit  : Unit_Type := Gold;
         begin
            if Index = Image'Last - 1
              and then Image (Image'Last) = 'p'
            then
               case Image (Index) is
                  when 'c' =>
                     Unit := Copper;
                  when 's' =>
                     Unit := Silver;
                  when 'g' =>
                     Unit := Gold;
                  when 'p' =>
                     Unit := Platinum;
                  when others =>
                     Unit := Gold;
               end case;
            end if;

            return Result : Coins_Type := (others => 0) do
               Result (Unit) := Count;
            end return;
         end;
      end if;

   end To_Coins;

   --------------------
   -- Total_Quantity --
   --------------------

   function Total_Quantity
     (Coins : Coins_Type)
      return Coin_Count
   is
      Result : Coin_Count := 0;
   begin
      for Unit in Coins'Range loop
         Result := Result + Coins (Unit);
      end loop;
      return Result;
   end Total_Quantity;

   -----------
   -- Value --
   -----------

   function Value
     (Coins : Coins_Type;
      Unit  : Unit_Type := Gold)
      return Coin_Value
   is
      Total_Copper : Coin_Value := 0;
   begin
      for U in Coins'Range loop
         Total_Copper := Total_Copper
           + Coins (U) * Copper_Value (U);
      end loop;
      return Coin_Value (Total_Copper / Copper_Value (Unit));
   end Value;

   ------------
   -- Weight --
   ------------

   function Weight (Coins : Coins_Type) return Natural is
   begin
      return Natural ((Total_Quantity (Coins) + 250) / 500);
   end Weight;

end Chaos.Coins;
