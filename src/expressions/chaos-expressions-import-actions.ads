with WL.Binary_IO;

package Chaos.Expressions.Import.Actions is

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
      return Chaos_Expression;

end Chaos.Expressions.Import.Actions;
