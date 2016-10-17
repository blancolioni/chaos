with Ada.Characters.Handling;

package body Chaos.Resources.Ids is

   ----------------
   -- Identifier --
   ----------------

   function Identifier
     (Ids   : Ids_Resource'Class;
      Index : Positive)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String
        (Ids.Id_Vector.Element (Index).Identifier);
   end Identifier;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Ids : in out Ids_Resource)
   is
      Start : Positive := 1;
   begin
      Chaos.Resources.Text.Text_Resource (Ids).Load;

      if Ids.Line_Count >= 1 then
         declare
            Header : constant String := Ids.Line (1);
         begin
            if Header'Length > 3 and then Header (1 .. 3) = "IDS" then
               Start := 2;
            end if;
         end;
      end if;

      for I in Start .. Ids.Line_Count loop
         declare
            use Ada.Characters.Handling;
            S     : constant String := Ids.Line (I + Start - 1);
            Index : Positive := S'First;
            Start : Positive;
            Value : Integer;
            Neg   : Boolean := False;
            Hex   : Boolean := False;
         begin
            if Index < S'Last then
               if S (Index) = '-' then
                  Index := Index + 1;
                  Neg := True;
               end if;
            end if;

            if Index < S'Last - 1
              and then S (Index .. Index + 1) = "0x"
            then
               Index := Index + 2;
               Hex := True;
            end if;

            Start := Index;

            if Index < S'Last
              and then not Is_Digit (S (Index))
              and then (not Hex
                        or else not Is_Hexadecimal_Digit (S (Index)))
            then
               raise Constraint_Error with
                 "bad line in IDS resource: " & S;
            end if;

            while Index <= S'Last
              and then (Is_Digit (S (Index))
                        or else (Hex
                                 and then Is_Hexadecimal_Digit (S (Index))))
            loop
               Index := Index + 1;
            end loop;

            if Index <= S'Last then
               if Hex then
                  Value :=
                    Integer'Value ("16#" & S (Start .. Index - 1) & "#");
               else
                  Value :=
                    Integer'Value (S (Start .. Index - 1));
               end if;

               if Neg then
                  Value := -Value;
               end if;

               while Is_Space (S (Index)) loop
                  Index := Index + 1;
               end loop;

               if Index < S'Last then
                  Ids.Id_Vector.Append
                    ((Ada.Strings.Unbounded.To_Unbounded_String
                     (S (Index .. S'Last)),
                     Value));
               end if;
            end if;
         end;
      end loop;
   end Load;

   ---------------
   -- Row_Count --
   ---------------

   function Row_Count
     (Ids : Ids_Resource'Class)
      return Natural
   is
   begin
      return Ids.Id_Vector.Last_Index;
   end Row_Count;

   -----------
   -- Value --
   -----------

   function Value
     (Ids     : Ids_Resource'Class;
      Index   : Positive)
      return Integer
   is
   begin
      return Ids.Id_Vector.Element (Index).Value;
   end Value;

end Chaos.Resources.Ids;
