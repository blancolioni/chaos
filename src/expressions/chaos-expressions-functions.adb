with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Chaos.Expressions.Maps;
with Chaos.Expressions.Primitives;

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
      Environment : Chaos_Environment;
      Arguments   : Array_Of_Expressions)
      return Chaos_Expression
   is (Evaluate (Environment, Expression.Value));

   overriding function To_String
     (Expression  : Assignment_Expression)
      return String
   is (To_String (Expression.Target)
       & "."
       & Ada.Strings.Unbounded.To_String (Expression.Name)
       & ":="
       & To_String (Expression.Value));

   type Function_Call_Expression is
     new Root_Chaos_Expression_Record with
      record
         Name      : Ada.Strings.Unbounded.Unbounded_String;
         Object    : Chaos_Expression;
         Arguments : Argument_Vectors.Vector;
      end record;

   overriding function Evaluate
     (Expression  : Function_Call_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression;

   overriding function To_Boolean
     (Expression  : Function_Call_Expression)
      return Boolean
   is (True);

   overriding function Apply
     (Expression  : Function_Call_Expression;
      Environment : Chaos_Environment;
      Arguments   : Array_Of_Expressions)
      return Chaos_Expression;

   overriding function To_String
     (Expression  : Function_Call_Expression)
      return String;

   type Lambda_Expression is
     new Root_Chaos_Expression_Record with
      record
         Arguments   : Argument_Vectors.Vector;
         Lambda_Body : Chaos_Expression;
      end record;

   overriding function Evaluate
     (Expression  : Lambda_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression
   is ((if Expression.Arguments.Last_Index = 0
        then Evaluate (Environment, Expression.Lambda_Body)
        else Create (Expression)));

   overriding function To_Boolean
     (Expression  : Lambda_Expression)
      return Boolean
   is (True);

   overriding function Apply
     (Expression  : Lambda_Expression;
      Environment : Chaos_Environment;
      Arguments   : Array_Of_Expressions)
      return Chaos_Expression;

   overriding function To_String
     (Expression  : Lambda_Expression)
      return String;

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (Expression  : Function_Call_Expression;
      Environment : Chaos_Environment;
      Arguments   : Array_Of_Expressions)
      return Chaos_Expression
   is
   begin
      return Get (Expression.Evaluate (Environment)).Apply
        (Environment, Arguments);
   end Apply;

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (Expression  : Lambda_Expression;
      Environment : Chaos_Environment;
      Arguments   : Array_Of_Expressions)
      return Chaos_Expression
   is
      Env : Chaos_Environment := New_Environment (Environment);
   begin
      for I in 1 .. Expression.Arguments.Last_Index loop
         if I in Arguments'Range then
            Insert (Env, To_String (Expression.Arguments (I)),
                    Arguments (I));
         else
            Insert (Env, To_String (Expression.Arguments (I)),
                    Undefined_Value);
         end if;
      end loop;
      return Evaluate (Env, Expression.Lambda_Body);
   end Apply;

   -----------------------
   -- Create_Assignment --
   -----------------------

   function Create_Assignment
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
   end Create_Assignment;

   --------------------------
   -- Create_Function_Call --
   --------------------------

   function Create_Function_Call
     (Function_Name : String;
      Arguments     : Array_Of_Expressions)
      return Chaos_Expression
   is
      Rec : Function_Call_Expression;
   begin
      Rec.Name := Ada.Strings.Unbounded.To_Unbounded_String (Function_Name);
      for Arg of Arguments loop
         Rec.Arguments.Append (Arg);
      end loop;
      return Create (Rec);
   end Create_Function_Call;

   ------------------------------
   -- Create_Lambda_Expression --
   ------------------------------

   function Create_Lambda_Expression
     (Arguments   : Array_Of_Expressions;
      Lambda_Body : Chaos_Expression)
      return Chaos_Expression
   is
      Rec : Lambda_Expression;
   begin
      for Arg of Arguments loop
         Rec.Arguments.Append (Arg);
      end loop;
      Rec.Lambda_Body := Lambda_Body;
      return Create (Rec);
   end Create_Lambda_Expression;

   ------------------------
   -- Create_Method_Call --
   ------------------------

   function Create_Method_Call
     (Object    : Chaos_Expression;
      Method    : String;
      Arguments : Array_Of_Expressions)
      return Chaos_Expression
   is
      Rec : Function_Call_Expression;
   begin
      Rec.Name := Ada.Strings.Unbounded.To_Unbounded_String (Method);
      Rec.Object := Object;
      for Arg of Arguments loop
         Rec.Arguments.Append (Arg);
      end loop;
      return Create (Rec);
   end Create_Method_Call;

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
                 Evaluate (Environment, Expression.Target);
      Value  : constant Chaos_Expression :=
                 Evaluate (Environment, Expression.Value);
   begin
      Set (Object).Set (To_String (Expression.Name),
                        Value);
      return Value;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Expression  : Function_Call_Expression;
      Environment : Chaos_Environment)
      return Chaos_Expression
   is
      Actuals : Array_Of_Expressions (1 .. Expression.Arguments.Last_Index);
      Name    : constant String :=
                  Ada.Strings.Unbounded.To_String (Expression.Name);
      Object  : constant Chaos_Expression :=
                  (if Is_Null (Expression.Object)
                   then Null_Value
                   else Evaluate (Environment, Expression.Object));
      Fn      : Chaos_Expression :=
                  (if Is_Null (Object)
                   then Find (Environment, Name)
                   else Find
                     (Get (Object).Local_Environment, Name));
   begin
      if Fn = Undefined_Value
        and then not Is_Null (Object)
        and then Chaos.Expressions.Maps.Is_Map (Object)
      then
         Fn := Chaos.Expressions.Maps.Get (Object, Name);
      end if;

      for I in Actuals'Range loop
         Actuals (I) :=
           Evaluate (Environment, Expression.Arguments.Element (I));
      end loop;

      if not Is_Null (Object) then
         if Actuals'Length = 0
           and then not Chaos.Expressions.Primitives.Is_Property (Fn)
         then
            return Fn;
         else
            return Get (Fn).Apply (Environment, Object & Actuals);
         end if;
      else
         return Get (Fn).Apply (Environment, Actuals);
      end if;
   end Evaluate;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Expression  : Function_Call_Expression)
      return String
   is
      use Ada.Strings.Unbounded;
      Args : Unbounded_String;
   begin
      if not Expression.Arguments.Is_Empty then
         for Arg of Expression.Arguments loop
            if Args /= Null_Unbounded_String then
               Args := Args & ",";
            end if;
            Args := Args & To_String (Arg);
         end loop;
         Args := "(" & Args & ")";
      end if;
      if not Is_Null (Expression.Object) then
         return To_String (Expression.Object) & "."
           & To_String (Expression.Name) & To_String (Args);
      else
         return To_String (Expression.Name) & To_String (Args);
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Expression  : Lambda_Expression)
      return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := To_Unbounded_String ("\");
   begin
      for Arg of Expression.Arguments loop
         Result := Result & " " & To_String (Arg);
      end loop;
      return To_String (Result & " -> " & To_String (Expression.Lambda_Body));
   end To_String;

end Chaos.Expressions.Functions;
