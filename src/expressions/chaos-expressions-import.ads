private with Ada.Containers.Indefinite_Vectors;
private with Ada.Strings.Unbounded;

with Chaos.Resources;

package Chaos.Expressions.Import is

   procedure Import_Script
     (Reference : Chaos.Resources.Resource_Reference);

   procedure Import_Scripts
     (Scripts : Chaos.Resources.Script_Array);

   type Function_Call is private;

   procedure Create_Function_Call
     (Call    : in out Function_Call;
      Name    : String;
      Negated : Boolean := False);

   procedure Add_String_Argument
     (Call : in out Function_Call;
      Text : String);

   procedure Add_Identifier_Argument
     (Call : in out Function_Call;
      Name : String);

   procedure Add_Integer_Argument
     (Call  : in out Function_Call;
      Value : Integer);

   procedure Add_Coordinate_Argument
     (Call  : in out Function_Call;
      X, Y  : Integer);

private

   type Argument_Type is (Integer_Argument, Text_Argument,
                          Identifier_Argument, Coordinate_Argument);

   type Actual_Argument (Arg_Type : Argument_Type) is
      record
         case Arg_Type is
            when Integer_Argument =>
               Integer_Value : Integer;
            when Text_Argument =>
               Text_Value    : Ada.Strings.Unbounded.Unbounded_String;
            when Identifier_Argument =>
               Identifier_Name : Ada.Strings.Unbounded.Unbounded_String;
            when Coordinate_Argument =>
               X, Y            : Integer;
         end case;
      end record;

   package Actual_Argument_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Actual_Argument);

   type Function_Call is
      record
         Name    : Ada.Strings.Unbounded.Unbounded_String;
         Args    : Actual_Argument_Vectors.Vector;
         Negated : Boolean;
      end record;

end Chaos.Expressions.Import;
