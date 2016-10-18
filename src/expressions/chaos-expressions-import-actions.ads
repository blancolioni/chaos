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

   procedure Import_Action
     (Call : Function_Call);

   function Get_Action_Id
     (Name : String)
      return Natural;

   type Action_Argument_Name is
     (Integer_1, Integer_2, Integer_3,
      Point_X, Point_Y,
      Text_1, Text_2,
      Object_1, Object_2, Object_3);

   subtype Object_Argument is Action_Argument_Name range Object_1 .. Object_3;

   type Action_Argument_Names is
     array (Positive range <>) of Action_Argument_Name;

   function Get_Action_Arguments
     (Id : Positive)
      return Action_Argument_Names;

end Chaos.Expressions.Import.Actions;
