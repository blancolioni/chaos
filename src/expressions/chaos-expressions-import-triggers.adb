with Chaos.Expressions.Functions;
with Chaos.Expressions.Identifiers;

with Chaos.Parser;

with Chaos.Logging;

package body Chaos.Expressions.Import.Triggers is

   use WL.Binary_IO;

   Global_Trigger : constant Word_32 := 16#400F#;

   --------------------
   -- Import_Trigger --
   --------------------

   function Import_Trigger
     (Trigger_Id : WL.Binary_IO.Word_32;
      Integer_1  : Integer;
      Flags      : WL.Binary_IO.Word_32;
      Integer_2  : Integer;
      Text_1     : String;
      Text_2     : String)
      return Chaos_Expression
   is
      pragma Unreferenced (Integer_2);
      pragma Unreferenced (Text_2);
      Result : Chaos_Expression := Never;
   begin
      case Trigger_Id is
         when Global_Trigger =>
            if Text_1'Length > 6 then
               return Chaos.Parser.Parse_Expression
                 (Text_1 (Text_1'First .. Text_1'First + 5) & "."
                  & Text_1 (Text_1'First + 6 .. Text_1'Last)
                  & " = " & Integer'Image (Integer_1));
            else
               return Chaos.Parser.Parse_Expression
                 (Text_1 & " = " & Integer'Image (Integer_1));
            end if;
         when others =>
            Chaos.Logging.Log ("SCRIPT", "unknown trigger" & Trigger_Id'Img);
            null;
      end case;

      if Flags mod 2 = 1 then
         Result :=
           Chaos.Expressions.Functions.Apply
             (Chaos.Expressions.Identifiers.To_Expression ("not"),
              Result);
      end if;

      return Result;

   end Import_Trigger;

end Chaos.Expressions.Import.Triggers;
