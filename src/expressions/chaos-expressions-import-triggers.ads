with WL.Binary_IO;

package Chaos.Expressions.Import.Triggers is

   procedure Load_Triggers;

   procedure Import_Trigger
     (Trigger_Id : Positive;
      Integer_1  : Integer;
      Flags      : WL.Binary_IO.Word_32;
      Integer_2  : Integer;
      Text_1     : String;
      Text_2     : String);

   procedure Import_Trigger
     (Call : Function_Call);

   function Get_Trigger_Id
     (Name : String)
      return Natural;

   type Trigger_Argument_Name is
     (Integer_1, Flags, Integer_2, Text_1, Text_2, Object_Reference);

   type Trigger_Argument_Names is
     array (Positive range <>) of Trigger_Argument_Name;

   function Get_Trigger_Arguments
     (Id : Positive)
      return Trigger_Argument_Names;

end Chaos.Expressions.Import.Triggers;
