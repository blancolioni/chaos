with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;

with WL.String_Maps;

with Chaos.Logging;

package body Chaos.Identifiers is

   type Identifier_Entry is
      record
         Identifier  : Ada.Strings.Unbounded.Unbounded_String;
         Group_Index : Positive;
         Value       : Integer;
      end record;

   package Identifier_Maps is
     new WL.String_Maps (Identifier_Entry);

   package Group_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   Identifier_Map : Identifier_Maps.Map;
   Group_Vector   : Group_Vectors.Vector;

   ---------
   -- Add --
   ---------

   procedure Add
     (Identifier : String;
      Value      : Integer;
      Group_Name : String)
   is
   begin
      if Identifier_Map.Contains (Identifier) then
         Chaos.Logging.Log
           ("IDS", "warning: " & Group_Name & "." & Identifier
            & " already exists as "
            & Group (Identifier) & "." & Identifier);
      else
         declare
            New_Entry : Identifier_Entry;
            Group_Index : Natural :=
                            Group_Vector.Find_Index (Group_Name);
         begin
            New_Entry.Identifier :=
              Ada.Strings.Unbounded.To_Unbounded_String (Identifier);
            New_Entry.Value := Value;
            if Group_Index = 0 then
               Group_Vector.Append (Group_Name);
               Group_Index := Group_Vector.Last_Index;
            end if;
            New_Entry.Group_Index := Group_Index;
            Identifier_Map.Insert (Identifier, New_Entry);
            Chaos.Logging.Log
              ("IDS", "added " & Group_Name & "." & Identifier & " = "
                 & Integer'Image (Value));
         end;
      end if;
   end Add;

   ------------
   -- Exists --
   ------------

   function Exists (Name : String) return Boolean is
   begin
      return Identifier_Map.Contains (Name);
   end Exists;

   -----------
   -- Group --
   -----------

   function Group (Name : String) return String is
   begin
      return Group_Vector.Element
        (Identifier_Map.Element (Name).Group_Index);
   end Group;

   -----------
   -- Value --
   -----------

   function Value (Name : String) return Integer is
   begin
      return Identifier_Map.Element (Name).Value;
   end Value;

end Chaos.Identifiers;
