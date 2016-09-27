with Chaos.Expressions;

package Chaos.Dice is

   function Roll
     (Count : Positive;
      Die   : Positive;
      Plus  : Integer)
      return Integer;

   function Roll
     (Die        : Positive)
      return Positive;

   type Die_Roll is private;

   function Create
     (Count : Natural;
      Die   : Natural;
      Plus  : Integer)
      return Die_Roll;

   function Create
     (Die        : Positive)
      return Die_Roll;

   function Roll
     (Die : Die_Roll)
      return Integer;

   function Maximum
     (Die : Die_Roll)
      return Integer;

   function Is_Die_Roll
     (Text : String)
      return Boolean;

   function Parse_Die_Roll
     (Text : String)
      return Die_Roll;

   function Show
     (Roll : Die_Roll)
      return String;

   function To_Expression
     (Roll : Die_Roll)
      return Chaos.Expressions.Chaos_Expression;

private

   type Die_Roll is
      record
         Count : Natural;
         Die   : Natural;
         Plus  : Integer;
      end record;

end Chaos.Dice;
