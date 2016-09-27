with Chaos.UI.Elements.Bins;

package Chaos.UI.Elements.Buttons is

   type Root_UI_Button is
     new Chaos.UI.Elements.Bins.Root_UI_Bin with private;

   type Button_Click_Handler is access
     procedure (Button : not null access Root_UI_Button'Class);

   procedure New_Button
     (Button   : in out Root_UI_Button'Class;
      Identity : String;
      Label    : String;
      On_Click : Button_Click_Handler);

   function New_Button
     (Identity : String;
      Label    : String;
      On_Click : Button_Click_Handler)
      return UI_Element;

   type UI_Button is access all Root_UI_Button'Class;

private

   type Root_UI_Button is
     new Chaos.UI.Elements.Bins.Root_UI_Bin with
      record
         On_Click : Button_Click_Handler;
      end record;

   overriding function Class_Name
     (Element : Root_UI_Button)
      return String
   is ("Button");

   overriding function Hierarchy
     (Element : Root_UI_Button)
      return String
   is ("Element Button");

end Chaos.UI.Elements.Buttons;
