with Ada.Strings.Unbounded;

with WL.String_Maps;

with Chaos.Expressions.Objects;

package body Chaos.Expressions.Maps is

   package Expression_Maps is
     new WL.String_Maps (Chaos_Expression);

   function To_String (Map : Expression_Maps.Map) return String;
   procedure Create_Virtual_Table (VT : in out Chaos_Environment);

   package Map_Objects is
     new Chaos.Expressions.Objects (Expression_Maps.Map,
                                    To_String, Create_Virtual_Table);

   --------------------------
   -- Create_Virtual_Table --
   --------------------------

   procedure Create_Virtual_Table (VT : in out Chaos_Environment) is
      pragma Unreferenced (VT);
   begin
      null;
   end Create_Virtual_Table;

   ---------
   -- Get --
   ---------

   function Get
     (Map : Chaos_Expression;
      Key : String)
      return Chaos_Expression
   is
      M : Expression_Maps.Map renames Map_Objects.To_Object (Map);
   begin
      if M.Contains (Key) then
         return M.Element (Key);
      else
         return Undefined_Value;
      end if;
   end Get;

   ------------
   -- Is_Map --
   ------------

   function Is_Map (Expression : Chaos_Expression) return Boolean is
   begin
      return Map_Objects.Is_Object (Expression);
   end Is_Map;

   --------------------
   -- Map_Expression --
   --------------------

   function Map_Expression return Chaos_Expression is
      Map : Expression_Maps.Map;
   begin
      return Map_Objects.To_Expression (Map);
   end Map_Expression;

   ---------
   -- Set --
   ---------

   procedure Set
     (Map    : Chaos_Expression;
      Key    : String;
      Value  : Chaos_Expression)
   is
      procedure Update (M : in out Expression_Maps.Map);

      ------------
      -- Update --
      ------------

      procedure Update (M : in out Expression_Maps.Map) is
      begin
         if M.Contains (Key) then
            M.Replace (Key, Value);
         else
            M.Insert (Key, Value);
         end if;
      end Update;
   begin
      Map_Objects.Update_Object (Map, Update'Access);
   end Set;

   ---------------
   -- To_String --
   ---------------

   function To_String (Map : Expression_Maps.Map) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for Position in Map.Iterate loop
         if Result /= Null_Unbounded_String then
            Result := Result & ",";
         end if;
         Result := Result & Expression_Maps.Key (Position)
           & ": " & To_String (Expression_Maps.Element (Position));
      end loop;
      return "{" & To_String (Result) & "}";
   end To_String;

end Chaos.Expressions.Maps;
