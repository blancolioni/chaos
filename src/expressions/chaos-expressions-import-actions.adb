with Chaos.Expressions.Functions;
with Chaos.Expressions.Identifiers;
with Chaos.Expressions.Numbers;

with Chaos.Parser;

package body Chaos.Expressions.Import.Actions is

   Display_String_Action    : constant := 151;
   Increment_Chapter_Action : constant := 161;

   -------------------
   -- Import_Action --
   -------------------

   function Import_Action
     (Action_Id  : WL.Binary_IO.Word_32;
      Object_1   : Chaos.Expressions.Chaos_Expression;
      Object_2   : Chaos.Expressions.Chaos_Expression;
      Object_3   : Chaos.Expressions.Chaos_Expression;
      Integer_1  : Integer;
      Integer_2  : Integer;
      Integer_3  : Integer;
      X, Y       : Integer;
      Text_1     : String;
      Text_2     : String)
      return Chaos_Expression
   is
      pragma Unreferenced (Object_1, Object_2, Object_3,
                           Integer_2, Integer_3, Y, X, Text_1, Text_2);
   begin
      case Action_Id is
         when Display_String_Action =>
            return Chaos.Expressions.Functions.Apply
              (Chaos.Expressions.Identifiers.To_Expression ("display-string"),
               Chaos.Expressions.Numbers.To_Expression (Integer_1));
         when Increment_Chapter_Action =>
            return Chaos.Parser.Parse_Expression
              ("global.chapter := global.chapter + 1");

         when others =>
            return Always;
      end case;
   end Import_Action;

end Chaos.Expressions.Import.Actions;
