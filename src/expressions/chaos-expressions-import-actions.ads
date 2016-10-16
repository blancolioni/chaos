with WL.Binary_IO;

package Chaos.Expressions.Import.Actions is

   procedure Load_Actions;

   procedure Import_Action
     (Action_Id  : WL.Binary_IO.Word_32;
      Integer_1  : Integer;
      Integer_2  : Integer;
      Integer_3  : Integer;
      X, Y       : Integer;
      Text_1     : String;
      Text_2     : String);

end Chaos.Expressions.Import.Actions;
