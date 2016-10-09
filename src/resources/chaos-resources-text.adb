package body Chaos.Resources.Text is

   type Array_Of_Bytes is array (Positive range <>) of Word_8;

   Xor_Key_Text : constant String :=
                    "88a88fba8ad3b9f5edb1cfeaaae4b5fb"
                    & "eb82f990cac9b5e7dc8eb7aceef7e0ca"
                    & "8eeaca80cec5adb7c4d08493d5f0ebc8"
                    & "b49dccafa595ba9987d29de391ba90ca";

   function To_Xor_Key
     (Text_Key : String)
      return Array_Of_Bytes;

   ----------
   -- Line --
   ----------

   function Line
     (Resource : Text_Resource;
      Index    : Positive)
      return String
   is
   begin
      return Resource.Lines (Index);
   end Line;

   ----------------
   -- Line_Count --
   ----------------

   function Line_Count (Resource : Text_Resource) return Natural is
   begin
      return Resource.Lines.Last_Index;
   end Line_Count;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Resource : in out Text_Resource)
   is
      Index    : Natural := 0;
      Got_NL   : Boolean := False;
      X        : Word_8;
      Buffer   : String (1 .. 100) := (others => ' ');
      Length   : Natural := 0;
      W        : Word_16;
      Key      : constant Array_Of_Bytes := To_Xor_Key (Xor_Key_Text);
      Key_Index : Natural := Key'Last;
      Encrypted : Boolean := False;
   begin
      Resource.Get (W);
      if W = 16#FFFF# then
         Encrypted := True;
      else
         Resource.Set_Offset (0);
      end if;

      while not Resource.End_Of_Resource loop
         Index := Index + 1;
         Resource.Get (X);
         if Encrypted then
            Key_Index := Key_Index + 1;
            if Key_Index > Key'Last then
               Key_Index := Key'First;
            end if;
            X := X xor Key (Key_Index);
         end if;

         if X = 10 or else X = 13 then
            if not Got_NL then
               Resource.Lines.Append (Buffer (1 .. Length));
               Length := 0;
            end if;
            Got_NL := True;
         else
            Got_NL := False;
            Length := Length + 1;
            Buffer (Length) := Character'Val (X);
         end if;
      end loop;
   end Load;

   ----------------
   -- To_Xor_Key --
   ----------------

   function To_Xor_Key
     (Text_Key : String)
      return Array_Of_Bytes
   is
      Result : Array_Of_Bytes (1 .. Text_Key'Length / 2);
      Count  : Natural := 0;
      High   : Boolean := True;
      X      : Word_8;
      D      : Word_8;
   begin
      for Ch of Text_Key loop
         D := (case Ch is
                  when '0' .. '9' =>
                     Character'Pos (Ch) - Character'Pos ('0'),
                  when 'a' .. 'f' =>
                     Character'Pos (Ch) - Character'Pos ('a') + 10,
                  when 'A' .. 'F' =>
                     Character'Pos (Ch) - Character'Pos ('A') + 10,
                  when others     => raise Constraint_Error);
         if High then
            X := D * 16;
         else
            X := X + D;
            Count := Count + 1;
            Result (Count) := X;
         end if;
         High := not High;
      end loop;
      return Result;
   end To_Xor_Key;

end Chaos.Resources.Text;
