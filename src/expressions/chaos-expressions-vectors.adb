with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package body Chaos.Expressions.Vectors is

   package Lith_Object_Vectors is
     new Ada.Containers.Vectors
       (Positive, Lith.Objects.Object, Lith.Objects."=");

   type Vector_Expression is
     new Lith.Objects.External_Object_Interface with
      record
         V : Lith_Object_Vectors.Vector;
      end record;

   overriding function Name
     (Vector : Vector_Expression)
      return String
   is ("vector");

   overriding function Print
     (Vector : Vector_Expression;
      Store  : in out Lith.Objects.Object_Store'Class)
      return String;

   overriding function Equal
     (X, Y : Vector_Expression;
      Store  : Lith.Objects.Object_Store'Class)
      return Boolean;

   overriding procedure Mark
     (Item  : in out Vector_Expression;
      Store : in out Lith.Objects.Object_Store'Class;
      Mark  : not null access
        procedure (X : in out Lith.Objects.Object));

   function Fetch
     (Item : Lith.Objects.Object)
      return access Vector_Expression'Class
   is (Vector_Expression (Store.Get_External_Object (Item).all)'Access);

   ------------
   -- Append --
   ------------

   procedure Append
     (To_Vector : Lith.Objects.Object;
      Value     : Lith.Objects.Object)
   is
   begin
      if Is_Vector (To_Vector) then
         Fetch (To_Vector).V.Append (Value);
      else
         raise Constraint_Error with
           "error: vector.append called on " & Store.Show (To_Vector)
           & "; value = " & Store.Show (Value);
      end if;
   end Append;

   ------------
   -- Create --
   ------------

   function Create
      return Lith.Objects.Object
   is
      Vector : Vector_Expression;
   begin
      return Store.Create_External_Reference (Vector);
   end Create;

   -----------
   -- Equal --
   -----------

   overriding function Equal
     (X, Y   : Vector_Expression;
      Store  : Lith.Objects.Object_Store'Class)
      return Boolean
   is
      use type Lith.Objects.Object;
      pragma Unreferenced (Store);
   begin
      if X.V.Last_Index /= Y.V.Last_Index then
         return False;
      end if;
      for I in 1 .. X.V.Last_Index loop
         if X.V.Element (I) /= Y.V.Element (I) then
            return False;
         end if;
      end loop;
      return True;
   end Equal;

   ---------
   -- Get --
   ---------

   function Get
     (Vector : Lith.Objects.Object;
      Index  : Positive)
      return Lith.Objects.Object
   is
   begin
      if Is_Vector (Vector) then
         return Fetch (Vector).V.Element (Index);
      else
         return Vector;
      end if;
   end Get;

   ---------------
   -- Is_Vector --
   ---------------

   function Is_Vector
     (Value : Lith.Objects.Object)
      return Boolean
   is
   begin
      return Lith.Objects.Is_External_Object (Value)
        and then Store.Get_External_Object (Value).all
      in Vector_Expression'Class;
   end Is_Vector;

   ------------
   -- Length --
   ------------

   function Length
     (Vector : Lith.Objects.Object)
      return Natural
   is
   begin
      if Is_Vector (Vector) then
         return Fetch (Vector).V.Last_Index;
      else
         return 1;
      end if;
   end Length;

   ----------
   -- Mark --
   ----------

   overriding procedure Mark
     (Item  : in out Vector_Expression;
      Store : in out Lith.Objects.Object_Store'Class;
      Mark  : not null access
        procedure (X : in out Lith.Objects.Object))
   is
      pragma Unreferenced (Store);
   begin
      for I in 1 .. Item.V.Last_Index loop
         Mark (Item.V (I));
      end loop;
   end Mark;

   -----------
   -- Print --
   -----------

   overriding function Print
     (Vector : Vector_Expression;
      Store  : in out Lith.Objects.Object_Store'Class)
      return String
   is
      use Ada.Strings.Unbounded;
      S : Unbounded_String;
   begin
      for I in 1 .. Vector.V.Last_Index loop
         if S /= Null_Unbounded_String then
            S := S & ", ";
         end if;
         S := S & Store.Show (Vector.V.Element (I));
      end loop;
      S := "[" & S & "]";
      return To_String (S);
   end Print;

   ---------
   -- Set --
   ---------

   procedure Set
     (Vector : Lith.Objects.Object;
      Index  : Positive;
      Value  : Lith.Objects.Object)
   is
   begin
      Fetch (Vector).V.Replace_Element (Index, Value);
   end Set;

end Chaos.Expressions.Vectors;
