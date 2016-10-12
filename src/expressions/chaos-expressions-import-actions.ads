with WL.Binary_IO;

package Chaos.Expressions.Import.Actions is

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
      Text_2     : String);

end Chaos.Expressions.Import.Actions;
