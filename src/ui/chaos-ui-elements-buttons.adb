with Chaos.UI.Elements.Labels;

package body Chaos.UI.Elements.Buttons is

   function On_Button_Click
     (Element : not null access Root_UI_Element'Class;
      Event   : Chaos.UI.Events.UI_Event)
     return Boolean;

   ----------------
   -- New_Button --
   ----------------

   procedure New_Button
     (Button   : in out Root_UI_Button'Class;
      Identity : String;
      Label    : String;
      On_Click : Button_Click_Handler)
   is
      Label_Element : constant UI_Element :=
                        Chaos.UI.Elements.Labels.New_Label
                          (Identity & "--label", Label);
   begin
      Button.New_Bin (Identity, Label_Element);
      Button.On_Click := On_Click;
      Button.Set_Handler (Chaos.UI.Events.Click, On_Button_Click'Access);
   end New_Button;

   ----------------
   -- New_Button --
   ----------------

   function New_Button
     (Identity : String;
      Label    : String;
      On_Click : Button_Click_Handler)
      return UI_Element
   is
      Button : constant UI_Button := new Root_UI_Button;
   begin
      Button.New_Button (Identity, Label, On_Click);
      return UI_Element (Button);
   end New_Button;

   ---------------------
   -- On_Button_Click --
   ---------------------

   function On_Button_Click
     (Element : not null access Root_UI_Element'Class;
      Event   : Chaos.UI.Events.UI_Event)
     return Boolean
   is
      pragma Unreferenced (Event);
   begin
      if Root_UI_Button (Element.all).On_Click /= null then
         Root_UI_Button (Element.all).On_Click
           (Root_UI_Button (Element.all)'Access);
         return True;
      else
         return False;
      end if;
   end On_Button_Click;

end Chaos.UI.Elements.Buttons;
