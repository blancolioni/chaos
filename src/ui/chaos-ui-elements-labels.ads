package Chaos.UI.Elements.Labels is

   type Root_UI_Label is new Root_UI_Element with private;

   function New_Label
     (Identity : String;
      Label    : String)
      return UI_Element;

private

   type Root_UI_Label is new Root_UI_Element with null record;

   overriding function Class_Name
     (Element : Root_UI_Label)
      return String
   is ("Label");

   overriding function Hierarchy
     (Element : Root_UI_Label)
      return String
   is ("Element Label");

   overriding function Sensitive
     (Element : Root_UI_Label;
      Event   : Chaos.UI.Events.UI_Event_Type)
      return Boolean
   is (False);

end Chaos.UI.Elements.Labels;
