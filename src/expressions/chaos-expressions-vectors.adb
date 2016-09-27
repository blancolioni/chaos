with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Chaos.Expressions.Objects;

package body Chaos.Expressions.Vectors is

   package Expression_Vectors is
     new Ada.Containers.Vectors (Positive, Chaos_Expression);

   function To_String (Vector : Expression_Vectors.Vector) return String;
   procedure Create_Virtual_Table (VT : in out Chaos_Environment);

   package Vector_Objects is
     new Chaos.Expressions.Objects (Expression_Vectors.Vector,
                                    To_String, Create_Virtual_Table);

   ------------
   -- Append --
   ------------

   procedure Append
     (To_Vector : Chaos_Expression;
      Value     : Chaos_Expression)
   is
      procedure Update (Vector : in out Expression_Vectors.Vector);

      ------------
      -- Update --
      ------------

      procedure Update (Vector : in out Expression_Vectors.Vector) is
      begin
         Vector.Append (Value);
      end Update;

   begin
      Vector_Objects.Update_Object (To_Vector, Update'Access);
   end Append;

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
     (Vector : Chaos_Expression;
      Index  : Positive)
      return Chaos_Expression
   is
   begin
      if Is_Vector (Vector) then
         return Vector_Objects.To_Object (Vector).Element (Index);
      elsif Index = 1 then
         return Vector;
      else
         raise Constraint_Error with "index out of range";
      end if;
   end Get;

   ---------------
   -- Is_Vector --
   ---------------

   function Is_Vector (Expression : Chaos_Expression) return Boolean is
   begin
      return Vector_Objects.Is_Object (Expression);
   end Is_Vector;

   ------------
   -- Length --
   ------------

   function Length (Vector : Chaos_Expression) return Natural is
   begin
      if Is_Vector (Vector) then
         return Vector_Objects.To_Object (Vector).Last_Index;
      else
         return 1;
      end if;
   end Length;

   ---------
   -- Set --
   ---------

   procedure Set
     (Vector : Chaos_Expression;
      Index  : Positive;
      Value  : Chaos_Expression)
   is
      procedure Update (V : in out Expression_Vectors.Vector);

      ------------
      -- Update --
      ------------

      procedure Update (V : in out Expression_Vectors.Vector) is
      begin
         V.Replace_Element (Index, Value);
      end Update;

   begin
      Vector_Objects.Update_Object (Vector, Update'Access);
   end Set;

   ---------------
   -- To_String --
   ---------------

   function To_String (Vector : Expression_Vectors.Vector) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for I in 1 .. Vector.Last_Index loop
         if Result /= Null_Unbounded_String then
            Result := Result & ",";
         end if;
         Result := Result & To_String (Vector.Element (I));
      end loop;
      return "[" & To_String (Result) & "]";
   end To_String;

   -----------------------
   -- Vector_Expression --
   -----------------------

   function Vector_Expression return Chaos_Expression is
      Vector : Expression_Vectors.Vector;
   begin
      return Vector_Objects.To_Expression (Vector);
   end Vector_Expression;

end Chaos.Expressions.Vectors;
