with Lith.Objects.Symbols;

with Chaos.Parser;

package body Chaos.Expressions.Import.Actions is

   Display_String_Action    : constant := 151;
   Increment_Chapter_Action : constant := 161;

   -------------------
   -- Import_Action --
   -------------------

   procedure Import_Action
     (Action_Id  : WL.Binary_IO.Word_32;
      Object_1   : Lith.Objects.Object;
      Object_2   : Lith.Objects.Object;
      Object_3   : Lith.Objects.Object;
      Integer_1  : Integer;
      Integer_2  : Integer;
      Integer_3  : Integer;
      X, Y       : Integer;
      Text_1     : String;
      Text_2     : String)
   is
      pragma Unreferenced (Object_1, Object_2, Object_3,
                           Integer_2, Integer_3, Y, X, Text_1, Text_2);
   begin
      case Action_Id is
         when Display_String_Action =>
            Store.Push (Lith.Objects.Symbols.Get_Symbol ("ui-display-string"));
            Store.Push (Integer_1);
            Store.Create_List (2);
         when Increment_Chapter_Action =>

            Chaos.Parser.Parse_Expression
              ("global.chapter := global.chapter + 1", Store.all);

         when others =>
            Store.Push (Lith.Objects.False_Value);
      end case;
   end Import_Action;

end Chaos.Expressions.Import.Actions;
