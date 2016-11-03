package Chaos.Identifiers is

   function Exists
     (Group_Name : String;
      Identifier : String)
      return Boolean;

   function Value
     (Group_Name : String;
      Identifier : String)
      return Integer
     with Pre => Exists (Group_Name, Identifier);

private

   procedure Add
     (Identifier : String;
      Value      : Integer;
      Group_Name : String);

end Chaos.Identifiers;
