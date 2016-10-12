with Lith.Objects.Symbols;

with Chaos.Parser;

with Chaos.Logging;

package body Chaos.Expressions.Import.Triggers is

   use WL.Binary_IO;

   Global_Trigger : constant Word_32 := 16#400F#;

   --------------------
   -- Import_Trigger --
   --------------------

   procedure Import_Trigger
     (Trigger_Id : WL.Binary_IO.Word_32;
      Integer_1  : Integer;
      Flags      : WL.Binary_IO.Word_32;
      Integer_2  : Integer;
      Text_1     : String;
      Text_2     : String)
   is
      pragma Unreferenced (Integer_2);
      pragma Unreferenced (Text_2);
   begin
      case Trigger_Id is
         when Global_Trigger =>
            if Text_1'Length > 6 then
               Chaos.Parser.Parse_Expression
                 (Text_1 (Text_1'First .. Text_1'First + 5) & "."
                  & Text_1 (Text_1'First + 6 .. Text_1'Last)
                  & " = " & Integer'Image (Integer_1),
                  Store.all);
            else
               Chaos.Parser.Parse_Expression
                 (Text_1 & " = " & Integer'Image (Integer_1),
                  Store.all);
            end if;
         when others =>
            Chaos.Logging.Log ("SCRIPT", "unknown trigger" & Trigger_Id'Img);
            if Flags mod 2 = 1 then
               Store.Push (Lith.Objects.True_Value);
            else
               Store.Push (Lith.Objects.False_Value);
            end if;
      end case;

      if Flags mod 2 = 1 then
         Store.Push (Lith.Objects.Symbols.Get_Symbol ("not"));
         Store.Swap;
         Store.Create_List (2);
      end if;

   end Import_Trigger;

end Chaos.Expressions.Import.Triggers;
