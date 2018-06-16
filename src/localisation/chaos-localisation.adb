with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Vectors;

with WL.String_Maps;

package body Chaos.Localisation is

   package Indexed_String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Local_Text_Index, String);

   package Localisation_Maps is
     new WL.String_Maps (String);

   Indexed_Strings  : Indexed_String_Vectors.Vector;
   Localisation_Map : Localisation_Maps.Map;

   --------------
   -- Add_Text --
   --------------

   procedure Add_Text
     (Key  : String;
      Text : String)
   is
   begin
      Localisation_Map.Insert (Key, Text);
   end Add_Text;

   ------------------
   -- Indexed_Text --
   ------------------

   function Indexed_Text
     (Index : Local_Text_Index)
      return String
   is
   begin
      return Indexed_Strings (Index);
   end Indexed_Text;

   ----------------
   -- Local_Text --
   ----------------

   function Local_Text
     (Key         : String;
      Capitalised : Boolean)
      return String
   is
   begin
      return Result : String := Local_Text (Key) do
         if Capitalised then
            Result (Result'First) :=
              Ada.Characters.Handling.To_Upper (Result (Result'First));
         else
            Result (Result'First) :=
              Ada.Characters.Handling.To_Lower (Result (Result'First));
         end if;
      end return;
   end Local_Text;

   ----------------
   -- Local_Text --
   ----------------

   function Local_Text
     (Key : String)
      return String
   is
   begin
      if Localisation_Map.Contains (Key) then
         return Localisation_Map.Element (Key);
      else
         return "[" & Key & "]";
      end if;
   end Local_Text;

   ----------------
   -- Local_Text --
   ----------------

   function Local_Text
     (Key   : String;
      Arg_1 : String;
      Arg_2 : String := "";
      Arg_3 : String := "";
      Arg_4 : String := "")
      return String
   is
      subtype Argument_Digit is Character range '1' .. '4';
      Text : constant String :=
               (if Localisation_Map.Contains (Key)
                then Localisation_Map.Element (Key)
                else "[" & Key & "]");

      function Apply (Start : Positive) return String;

      -----------
      -- Apply --
      -----------

      function Apply (Start : Positive) return String is
      begin
         if Start < Text'Last - 1
           and then Text (Start) = '{'
           and then Text (Start + 1) in Argument_Digit
           and then Text (Start + 2) = '}'
         then
            declare
               New_Text : constant String :=
                            (case Argument_Digit (Text (Start + 1)) is
                                when '1' => Arg_1,
                                when '2' => Arg_2,
                                when '3' => Arg_3,
                                when '4' => Arg_4);
            begin
               return New_Text & Apply (Start + 3);
            end;
         elsif Start < Text'Last then
            return Text (Start) & Apply (Start + 1);
         else
            return Text (Start .. Text'Last);
         end if;
      end Apply;

   begin
      return Apply (Text'First);
   end Local_Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Index : Local_Text_Index;
      Text  : String)
   is
   begin
      while Indexed_Strings.Last_Index < Index - 1 loop
         Indexed_Strings.Append ("");
      end loop;
      Indexed_Strings.Append (Text);
   end Set_Text;

end Chaos.Localisation;
