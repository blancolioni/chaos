package body Chaos.Expressions.Import.Objects is

   -------------------
   -- Import_Object --
   -------------------

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
      return Chaos_Expression
   is
      pragma Unreferenced (Team, Faction, EA, General, Race, Class,
                           Specific, Gender, Alignment,
                           Id_1, Id_2, Id_3, Id_4, Id_5, Name);
   begin
      return Null_Value;
   end Import_Object;

end Chaos.Expressions.Import.Objects;
