package Chaos.Localisation is

   type Local_Text_Index is new Natural;

   function Local_Text
     (Key : String)
      return String;

   function Local_Text
     (Key : String;
      Arg_1 : String;
      Arg_2 : String := "";
      Arg_3 : String := "";
      Arg_4 : String := "")
      return String;

   function Local_Text
     (Key         : String;
      Capitalised : Boolean)
      return String;

   function Capitalised_Local_Text
     (Key         : String)
      return String
   is (Local_Text (Key, Capitalised => True));

   function Indexed_Text
     (Index : Local_Text_Index)
      return String;

   procedure Set_Text
     (Index : Local_Text_Index;
      Text  : String);

private

   procedure Add_Text
     (Key  : String;
      Text : String);

end Chaos.Localisation;
