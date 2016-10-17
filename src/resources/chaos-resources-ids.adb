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
   begin
      Chaos.Resources.Text.Text_Resource (Ids).Load;

      declare
         Count_Line : constant String := Ids.Line (1);
         Count      : constant Natural :=
                        (if Count_Line = ""
                         then Ids.Line_Count - 1
                         else Natural'Value (Count_Line));
      begin
         for I in 1 .. Count loop
            declare
               use Ada.Characters.Handling;
               S : constant String := Ids.Line (1 + I);
               Index : Positive := S'First;
               Value : Integer;
            begin
               if S (Index) = '-' then
                  Index := Index + 1;
               end if;
               if not Is_Digit (S (Index)) then
                  raise Constraint_Error with
                    "bad line in IDS resource: " & S;
               end if;

               while Is_Digit (S (Index)) loop
                  Index := Index + 1;
               end loop;
               Value := Integer'Value (S (S'First .. Index - 1));
               while Is_Space (S (Index)) loop
                  Index := Index + 1;
               end loop;
               Ids.Id_Vector.Append
                 ((Ada.Strings.Unbounded.To_Unbounded_String
                  (S (Index .. S'Last)),
                  Value));
            end;
         end loop;
      end;
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
