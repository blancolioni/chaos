package body Chaos.UI.Elements.Labels is

   ---------------
   -- New_Label --
   ---------------

   function New_Label
     (Identity : String;
      Label    : String)
      return UI_Element
   is
      Result : Root_UI_Label;
   begin
      Result.Create (Identity);
      Result.Set_Text (Label);
      return new Root_UI_Label'(Result);
   end New_Label;

end Chaos.UI.Elements.Labels;
