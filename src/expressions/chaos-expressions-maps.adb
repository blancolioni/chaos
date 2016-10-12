with Ada.Strings.Unbounded;

with WL.String_Maps;

package body Chaos.Expressions.Maps is

   package Lith_Object_Maps is
     new WL.String_Maps
       (Lith.Objects.Object, Lith.Objects."=");

   type Map_Expression is
     new Lith.Objects.External_Object_Interface with
      record
         M : Lith_Object_Maps.Map;
      end record;

   overriding function Name
     (Map : Map_Expression)
      return String
   is ("Map");

   overriding function Print
     (Map : Map_Expression;
      Store  : in out Lith.Objects.Object_Store'Class)
      return String;

   overriding function Equal
     (X, Y : Map_Expression;
      Store  : Lith.Objects.Object_Store'Class)
      return Boolean;

   overriding procedure Mark
     (Item  : in out Map_Expression;
      Store : in out Lith.Objects.Object_Store'Class;
      Mark  : not null access
        procedure (X : in out Lith.Objects.Object));

   function Fetch
     (Item : Lith.Objects.Object)
      return access Map_Expression'Class
   is (Map_Expression (Store.Get_External_Object (Item).all)'Access);

   ------------
   -- Create --
   ------------

   function Create
      return Lith.Objects.Object
   is
      Map : Map_Expression;
   begin
      return Store.Create_External_Reference (Map);
   end Create;

   -----------
   -- Equal --
   -----------

   overriding function Equal
     (X, Y   : Map_Expression;
      Store  : Lith.Objects.Object_Store'Class)
      return Boolean
   is
      use type Lith_Object_Maps.Map;
      pragma Unreferenced (Store);
   begin
      return X.M = Y.M;
   end Equal;

   ---------
   -- Get --
   ---------

   function Get
     (Map    : Lith.Objects.Object;
      Key    : String)
      return Lith.Objects.Object
   is
   begin
      if Fetch (Map).M.Contains (Key) then
         return Fetch (Map).M.Element (Key);
      else
         return Lith.Objects.Undefined;
      end if;
   end Get;

   ------------
   -- Is_Map --
   ------------

   function Is_Map
     (Value : Lith.Objects.Object)
      return Boolean
   is
   begin
      return Lith.Objects.Is_External_Object (Value)
        and then Store.Get_External_Object (Value).all
      in Map_Expression'Class;
   end Is_Map;

   ----------
   -- Mark --
   ----------

   overriding procedure Mark
     (Item  : in out Map_Expression;
      Store : in out Lith.Objects.Object_Store'Class;
      Mark  : not null access
        procedure (X : in out Lith.Objects.Object))
   is
      use Lith_Object_Maps;
      pragma Unreferenced (Store);
      Position : Cursor := Item.M.First;
   begin
      while Has_Element (Position) loop
         declare
            Value     : Lith.Objects.Object :=
                          Element (Position);
         begin
            Mark (Value);
            Item.M.Replace_Element (Position, Value);
         end;
         Next (Position);
      end loop;
   end Mark;

   -----------
   -- Print --
   -----------

   overriding function Print
     (Map : Map_Expression;
      Store  : in out Lith.Objects.Object_Store'Class)
      return String
   is
      use Ada.Strings.Unbounded;
      use Lith_Object_Maps;
      S : Unbounded_String;
      Position : Cursor := Map.M.First;
   begin
      while Has_Element (Position) loop
         declare
            Value_Key : constant String := Key (Position);
            Value     : constant Lith.Objects.Object := Element (Position);
         begin
            if S /= Null_Unbounded_String then
               S := S & ", ";
            end if;
            S := S & Value_Key & ": " & Store.Show (Value);
         end;
         Next (Position);
      end loop;
      S := "{" & S & "}";
      return To_String (S);
   end Print;

   ---------
   -- Set --
   ---------

   procedure Set
     (Map    : Lith.Objects.Object;
      Key    : String;
      Value  : Lith.Objects.Object)
   is
   begin
      if Fetch (Map).M.Contains (Key) then
         Fetch (Map).M.Replace (Key, Value);
      else
         Fetch (Map).M.Insert (Key, Value);
      end if;
   end Set;

end Chaos.Expressions.Maps;
