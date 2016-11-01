private with Ada.Containers.Indefinite_Holders;
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

   type Script_Tuple is array (Positive range <>) of Natural;

   procedure Add_Tuple_Argument
     (Call  : in out Function_Call;
      Tuple : Script_Tuple);

private

   type Argument_Type is (Integer_Argument, Text_Argument,
                          Identifier_Argument, Tuple_Argument);

   package Tuple_Holder is
     new Ada.Containers.Indefinite_Holders (Script_Tuple);

   type Actual_Argument (Arg_Type : Argument_Type) is
      record
         case Arg_Type is
            when Integer_Argument =>
               Integer_Value : Integer;
            when Text_Argument =>
               Text_Value    : Ada.Strings.Unbounded.Unbounded_String;
            when Identifier_Argument =>
               Identifier_Name : Ada.Strings.Unbounded.Unbounded_String;
            when Tuple_Argument =>
               Tuple           : Tuple_Holder.Holder;
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
