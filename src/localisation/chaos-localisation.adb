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
