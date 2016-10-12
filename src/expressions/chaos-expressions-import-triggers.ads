with WL.Binary_IO;

package Chaos.Expressions.Import.Triggers is

   procedure Import_Trigger
     (Trigger_Id : WL.Binary_IO.Word_32;
      Integer_1  : Integer;
      Flags      : WL.Binary_IO.Word_32;
      Integer_2  : Integer;
      Text_1     : String;
      Text_2     : String);

end Chaos.Expressions.Import.Triggers;
