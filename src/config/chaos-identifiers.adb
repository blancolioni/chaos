with WL.String_Maps;

package body Chaos.Identifiers is

   package Identifier_Maps is
     new WL.String_Maps (Integer);

   type Group_Entry is
      record
         Id_Map : Identifier_Maps.Map;
      end record;

   package Group_Maps is
     new WL.String_Maps (Group_Entry);

   Group_Map : Group_Maps.Map;

   ---------
   -- Add --
   ---------

   procedure Add
     (Identifier : String;
      Value      : Integer;
      Group_Name : String)
   is
      Map : Group_Entry;
   begin
      if Group_Map.Contains (Group_Name) then
         Map := Group_Map.Element (Group_Name);
      end if;

      if Map.Id_Map.Contains (Identifier) then
         raise Constraint_Error with
           "identifier '" & Identifier & "' already exists in group '"
           & Group_Name & "'";
      end if;

      Map.Id_Map.Insert (Identifier, Value);

      if Group_Map.Contains (Group_Name) then
         Group_Map.Replace (Group_Name, Map);
      else
         Group_Map.Insert (Group_Name, Map);
      end if;

   end Add;

   ------------
   -- Exists --
   ------------

   function Exists
     (Group_Name : String;
      Identifier : String)
      return Boolean
   is
   begin
      return Group_Map.Contains (Group_Name)
        and then Group_Map (Group_Name).Id_Map.Contains (Identifier);
   end Exists;

   -----------
   -- Value --
   -----------

   function Value
     (Group_Name : String;
      Identifier : String)
      return Integer
   is
   begin
      return Group_Map (Group_Name).Id_Map (Identifier);
   end Value;

end Chaos.Identifiers;
