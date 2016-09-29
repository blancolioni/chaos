with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Chaos.Expressions.Maps;

package body Chaos.Expressions.Functions is

   package Argument_Vectors is
     new Ada.Containers.Vectors (Positive, Chaos_Expression);

   type Assignment_Expression is
     new Root_Chaos_Expression_Record with
      record
         Target : Chaos_Expression;
         Name   : Ada.Strings.Unbounded.Unbounded_String;
         Value  : Chaos_Expression;
      end record;

   overriding function Evaluate
     (Expression  : Assignment_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression;

   overriding function To_Boolean
     (Expression  : Assignment_Expression)
      return Boolean
   is (False);

   overriding function Apply
     (Expression  : Assignment_Expression;
      Argument    : Chaos_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression
   is (raise Constraint_Error with "cannot apply an assignment");

   overriding function To_String
     (Expression  : Assignment_Expression)
      return String
   is (To_String (Expression.Target)
       & "."
       & Ada.Strings.Unbounded.To_String (Expression.Name)
       & ":="
       & To_String (Expression.Value));

   type Application_Expression is
     new Root_Chaos_Expression_Record with
      record
         Fun : Chaos_Expression;
         Arg : Chaos_Expression;
      end record;

   overriding function Evaluate
     (Expression  : Application_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression;

   overriding function To_Boolean
     (Expression  : Application_Expression)
      return Boolean
   is (True);

   overriding function Apply
     (Expression  : Application_Expression;
      Argument    : Chaos_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression;

   overriding function To_String
     (Expression  : Application_Expression)
      return String;

   type Method_Expression is
     new Root_Chaos_Expression_Record with
      record
         Source : Chaos_Expression;
         Name   : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Evaluate
     (Expression  : Method_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression;

   overriding function To_Boolean
     (Expression  : Method_Expression)
      return Boolean
   is (True);

   overriding function Apply
     (Expression  : Method_Expression;
      Argument    : Chaos_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression
   is (raise Constraint_Error with "cannot apply a method");

   overriding function To_String
     (Expression  : Method_Expression)
      return String
   is (To_String (Expression.Source)
       & "."
       & Ada.Strings.Unbounded.To_String (Expression.Name));

   type Lambda_Expression is
     new Root_Chaos_Expression_Record with
      record
         Argument    : Ada.Strings.Unbounded.Unbounded_String;
         Lambda_Body : Chaos_Expression;
      end record;

   overriding function Is_Atom
     (Expression : Lambda_Expression)
      return Boolean
   is (False);

   overriding function Evaluate
     (Expression  : Lambda_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression
   is (Create (Expression));

   overriding function To_Boolean
     (Expression  : Lambda_Expression)
      return Boolean
   is (True);

   overriding function Apply
     (Expression  : Lambda_Expression;
      Argument    : Chaos_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression;

   overriding function To_String
     (Expression  : Lambda_Expression)
      return String;

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (Expression  : Application_Expression;
      Argument    : Chaos_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression
   is
      pragma Unreferenced (Environment);
   begin
      return Apply (Create (Expression), Argument);
   end Apply;

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (Expression  : Lambda_Expression;
      Argument    : Chaos_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression
   is
      use Ada.Strings.Unbounded;
      Env : Chaos_Environment := New_Environment (Environment);
   begin
      Insert (Env, To_String (Expression.Argument), Argument);
      return Evaluate (Expression.Lambda_Body, Env);
   end Apply;

   -----------
   -- Apply --
   -----------

   function Apply
     (Expression : Chaos_Expression;
      Argument   : Chaos_Expression)
      return Chaos_Expression
   is
      Rec : Application_Expression;
   begin
      Rec.Fun := Expression;
      Rec.Arg := Argument;
      return Create (Rec);
   end Apply;

   -----------------------
   -- Create_Assignment --
   -----------------------

   function Assign
     (Object    : Chaos_Expression;
      Name      : String;
      Value     : Chaos_Expression)
      return Chaos_Expression
   is
      Rec : Assignment_Expression;
   begin
      Rec.Target := Object;
      Rec.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Rec.Value := Value;
      return Create (Rec);
   end Assign;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Expression  : Assignment_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression
   is
      use Ada.Strings.Unbounded;
      Object : constant Chaos_Expression :=
                 Evaluate (Expression.Target, Environment);
      Value  : constant Chaos_Expression :=
                 Evaluate (Expression.Value, Environment);
   begin
      Set (Object).Set (To_String (Expression.Name),
                        Value);
      return Value;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Expression  : Application_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression
   is
      Fun : constant Chaos_Expression :=
              Evaluate (Expression.Fun, Environment);
      Arg : constant Chaos_Expression :=
              Evaluate (Expression.Arg, Environment);
   begin
      return Get (Fun).Apply (Arg, Environment);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Expression  : Method_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression
   is
      Object : constant Chaos_Expression :=
                 Evaluate (Expression.Source, Environment);
      Env : Chaos_Environment renames
                 Get (Object).Local_Environment;
      Name : constant String :=
                 Ada.Strings.Unbounded.To_String (Expression.Name);
      Result : Chaos_Expression;
   begin
      if Contains (Env, Name) then
         Result := Find (Env, Name);
      elsif Chaos.Expressions.Maps.Is_Map (Object) then
         Result := Chaos.Expressions.Maps.Get (Object, Name);
      else
         Result := Undefined_Value;
      end if;
      if Get (Result).Is_Function then
         Result := Evaluate (Apply (Result, Object), Environment);
      end if;
      return Result;
   end Evaluate;

   ------------
   -- Lambda --
   ------------

   function Lambda
     (Argument    : String;
      Lambda_Body : Chaos_Expression)
      return Chaos_Expression
   is
      Rec : Lambda_Expression;
   begin
      Rec.Argument := Ada.Strings.Unbounded.To_Unbounded_String (Argument);
      Rec.Lambda_Body := Lambda_Body;
      return Create (Rec);
   end Lambda;

   -------------------
   -- Object_Method --
   -------------------

   function Object_Method
     (Object    : Chaos_Expression;
      Method    : String)
      return Chaos_Expression
   is
      Rec : Method_Expression;
   begin
      Rec.Source := Object;
      Rec.Name := Ada.Strings.Unbounded.To_Unbounded_String (Method);
      return Create (Rec);
   end Object_Method;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Expression  : Application_Expression)
      return String
   is
      Left  : constant String := To_String (Expression.Fun);
      Right : constant String := To_String (Expression.Arg);
   begin
      if Get (Expression.Arg) in Application_Expression'Class then
         return Left & " (" & Right & ")";
      else
         return Left & " " & Right;
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Expression  : Lambda_Expression)
      return String
   is
   begin
      return "\" & Ada.Strings.Unbounded.To_String (Expression.Argument)
        & " => " & To_String (Expression.Lambda_Body);
   end To_String;

end Chaos.Expressions.Functions;
