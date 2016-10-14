with Lith.Objects.Symbols;

with Chaos.Logging;
with Chaos.Parser;

package body Chaos.Expressions.Import.Actions is

   Create_Creature_Action   : constant := 7;
   Continue_Action          : constant := 36;
   Start_Timer_Action       : constant := 61;
   Set_Global_Timer         : constant := 115;
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
                           Integer_3, Text_2);
   begin
      case Action_Id is
         when Create_Creature_Action =>
            Chaos.Parser.Parse_Expression
              ("chaos-create-actor this '" & Text_1 & " "
               & Integer'Image (X)
               & " "
               & Integer'Image (Y)
               & Integer'Image (Integer_1));
         when Continue_Action =>
            Store.Push (Lith.Objects.Symbols.Quote_Symbol);
            Store.Push (Lith.Objects.Symbols.Get_Symbol ("continue"));
            Store.Create_List (2);
         when Start_Timer_Action =>
            Chaos.Parser.Parse_Expression
              ("chaos-set-timer this " & Integer'Image (Integer_1)
               & Integer'Image (Integer_2));
         when Set_Global_Timer =>
            Store.Push (Lith.Objects.Symbols.Get_Symbol ("chaos-set-timer"));
            Store.Push (Lith.Objects.Symbols.Quote_Symbol);
            Store.Push
              (Lith.Objects.Symbols.Get_Symbol
                 (Text_1 (Text_1'First .. Text_1'First + 5)));
            Store.Create_List (2);
            Store.Push (Lith.Objects.Symbols.Quote_Symbol);
            Store.Push
              (Lith.Objects.Symbols.Get_Symbol
                 (Text_1 (Text_1'First + 6 .. Text_1'Last)));
            Store.Create_List (2);
            Store.Push (Integer_1);
            Store.Create_List (4);
         when Display_String_Action =>
            Store.Push (Lith.Objects.Symbols.Get_Symbol ("ui-display-string"));
            Store.Push (Integer_1);
            Store.Create_List (2);
         when Increment_Chapter_Action =>

            Chaos.Parser.Parse_Expression
              ("global.chapter := global.chapter + 1");

         when others =>
            Chaos.Logging.Log ("SCRIPT", "unknown action" & Action_Id'Img);
            Store.Push (Lith.Objects.False_Value);
      end case;
   end Import_Action;

end Chaos.Expressions.Import.Actions;
