package Chaos.Expressions.Import.Objects is

   procedure Load_Objects;

   procedure Import_Object
     (Team      : Natural;
      Faction   : Natural;
      EA        : Natural;
      General   : Natural;
      Race      : Natural;
      Class     : Natural;
      Specific  : Natural;
      Gender    : Natural;
      Alignment : Natural;
      Id_1      : Natural;
      Id_2      : Natural;
      Id_3      : Natural;
      Id_4      : Natural;
      Id_5      : Natural;
      Name      : String);

   procedure Import_Object_Identifier
     (Id : String);

   procedure Import_Object_Name
     (Name : String);

   procedure Import_Object_Tuple
     (Tuple : Script_Tuple);

   subtype Object_Tuple_Index is Integer range 1 .. 7;

   function Group_Name
     (Tuple_Index : Object_Tuple_Index)
      return String;

end Chaos.Expressions.Import.Objects;
