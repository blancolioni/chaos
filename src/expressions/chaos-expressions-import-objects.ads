package Chaos.Expressions.Import.Objects is

   function Import_Object
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
      Name      : String)
      return Chaos_Expression;

end Chaos.Expressions.Import.Objects;
