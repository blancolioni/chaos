package Chaos.Identifiers is

   function Exists (Name : String) return Boolean;

   function Value (Name : String) return Integer
     with Pre => Exists (Name);

   function Group (Name : String) return String
     with Pre => Exists (Name);

private

   procedure Add
     (Identifier : String;
      Value      : Integer;
      Group_Name : String);

end Chaos.Identifiers;
