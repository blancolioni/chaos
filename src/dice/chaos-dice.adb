with Ada.Characters.Handling;

with WL.Random;

with Chaos.Expressions.Numbers;
with Chaos.Expressions.Objects;
with Chaos.Expressions.Primitives;

package body Chaos.Dice is

   procedure Create_VT (VT : in out Chaos.Expressions.Chaos_Environment);

   package Dice_Expressions is
     new Chaos.Expressions.Objects (Die_Roll, Show, Create_VT);

   function Primitive_Roll_Dice
     (Environment : Chaos.Expressions.Chaos_Environment;
      Arguments   : Chaos.Expressions.Array_Of_Expressions)
      return Chaos.Expressions.Chaos_Expression;

   ------------
   -- Create --
   ------------

   function Create
     (Count : Natural;
      Die   : Natural;
      Plus  : Integer)
      return Die_Roll
   is
   begin
      return (Count, Die, Plus);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Die : Positive)
      return Die_Roll
   is
   begin
      return Create (1, Die, 0);
   end Create;

   ---------------
   -- Create_VT --
   ---------------

   procedure Create_VT (VT : in out Chaos.Expressions.Chaos_Environment) is
   begin
      Chaos.Expressions.Insert
        (VT, "roll",
         Chaos.Expressions.Primitives.Bind_Function
           (Primitive_Roll_Dice'Access, 1));
      Chaos.Expressions.Insert
        (VT, "to_integer",
         Chaos.Expressions.Primitives.Bind_Function
           (Primitive_Roll_Dice'Access, 1));
   end Create_VT;

   function Is_Die_Roll
     (Text : String)
      return Boolean
   is
      use Ada.Characters.Handling;

      type Parse_State is
        (First_Number, D, Second_Number, Plus_Minus, Last_Number);

      State : Parse_State := First_Number;

   begin
      for I in Text'Range loop
         if Is_Digit (Text (I)) then
            case State is
               when First_Number | Second_Number | Last_Number =>
                  null;
               when D =>
                  State := Second_Number;
               when Plus_Minus =>
                  State := Last_Number;
            end case;
         elsif Text (I) = 'd' then
            if State = First_Number then
               State := D;
            else
               return False;
            end if;
         elsif Text (I) = '+' or else Text (I) = '-' then
            if State = D then
               State := Plus_Minus;
            else
               return False;
            end if;
         else
            return False;
         end if;
      end loop;
      return State = Second_Number or else State = Last_Number;
   end Is_Die_Roll;

   -------------
   -- Maximum --
   -------------

   function Maximum
     (Die : Die_Roll)
      return Integer
   is
   begin
      return Die.Count * Die.Die + Die.Plus;
   end Maximum;

   --------------------
   -- Parse_Die_Roll --
   --------------------

   function Parse_Die_Roll
     (Text : String)
      return Die_Roll
   is
      use Ada.Characters.Handling;
      Index   : Positive := Text'First;

      Result : Die_Roll := (1, 0, 0);

      function Scan_Number (Default : Positive := 1) return Natural;

      -----------------
      -- Scan_Number --
      -----------------

      function Scan_Number (Default : Positive := 1) return Natural is
         X : Natural := 0;
      begin
         if Is_Digit (Text (Index)) then
            while Index <= Text'Last and then Is_Digit (Text (Index)) loop
               X := X * 10
                 + Character'Pos (Text (Index)) - Character'Pos ('0');
               Index := Index + 1;
            end loop;
         else
            X := Default;
         end if;
         return X;
      end Scan_Number;

   begin
      declare
         Start : constant Natural := Scan_Number;
      begin
         if Index > Text'Last then
            Result.Plus := Start;
         else
            Result.Count := Start;
            if Index <= Text'Last - 1
              and then Text (Index) = 'd'
              and then Is_Digit (Text (Index + 1))
            then
               Index := Index + 1;
               Result.Die := Scan_Number (6);
            end if;

            if Index <= Text'Last then
               if Text (Index) = '+' then
                  Index := Index + 1;
                  Result.Plus := Scan_Number;
               elsif Text (Index) = '-' then
                  Index := Index + 1;
                  Result.Plus := -1 * Scan_Number;
               end if;
            end if;
         end if;
      end;

      return Result;

   end Parse_Die_Roll;

   -------------------------
   -- Primitive_Roll_Dice --
   -------------------------

   function Primitive_Roll_Dice
     (Environment : Chaos.Expressions.Chaos_Environment;
      Arguments   : Chaos.Expressions.Array_Of_Expressions)
      return Chaos.Expressions.Chaos_Expression
   is
      pragma Unreferenced (Environment);
      pragma Assert (Arguments'Length = 1);
      pragma Assert (Dice_Expressions.Is_Object (Arguments (Arguments'First)));
      Dice : constant Die_Roll :=
               Dice_Expressions.To_Object
                 (Arguments (Arguments'First));
      Result : constant Integer := Roll (Dice);
   begin
      return Chaos.Expressions.Numbers.To_Expression (Result);
   end Primitive_Roll_Dice;

   ----------
   -- Roll --
   ----------

   function Roll
     (Count : Positive;
      Die   : Positive;
      Plus  : Integer)
      return Integer
   is
   begin
      return Result : Integer := 0 do
         for I in 1 .. Count loop
            Result := Result + Roll (Die);
         end loop;
         Result := Result + Plus;
      end return;
   end Roll;

   ----------
   -- Roll --
   ----------

   function Roll
     (Die        : Positive)
      return Positive
   is
   begin
      return WL.Random.Random_Number (1, Die);
   end Roll;

   ----------
   -- Roll --
   ----------

   function Roll
     (Die : Die_Roll)
      return Integer
   is
   begin
      if Die.Count = 0 then
         return Die.Plus;
      else
         return Roll (Die.Count, Die.Die, Die.Plus);
      end if;
   end Roll;

   ----------
   -- Show --
   ----------

   function Show
     (Roll : Die_Roll)
      return String
   is
      M : constant String := Natural'Image (Roll.Count);
      D : constant String := Natural'Image (Roll.Die);
      P : constant String := Integer'Image (Roll.Plus);
   begin
      if Roll.Die = 0 then
         if Roll.Plus < 0 then
            return P;
         else
            return P (2 .. P'Last);
         end if;
      elsif Roll.Plus < 0 then
         return M (2 .. M'Last) & "d" & D (2 .. D'Last) & P;
      elsif Roll.Plus = 0 then
         return M (2 .. M'Last) & "d" & D (2 .. D'Last);
      else
         return M (2 .. M'Last) & "d" & D (2 .. D'Last) & "+"
           & P (2 .. P'Last);
      end if;
   end Show;

   -------------------
   -- To_Expression --
   -------------------

   function To_Expression
     (Roll : Die_Roll)
      return Chaos.Expressions.Chaos_Expression
   is
   begin
      return Dice_Expressions.To_Expression (Roll);
   end To_Expression;

end Chaos.Dice;
