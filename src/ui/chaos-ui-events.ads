package Chaos.UI.Events is

   type Mouse_Button is (Left, Middle, Right);

   type UI_Event_Type is (Button_Press, Button_Release, Click,
                          Mouse_Enter, Mouse_Leave);

   type Array_Of_Event_Types is array (Positive range <>) of UI_Event_Type;

   type UI_Event (Event_Type : UI_Event_Type) is private;

   function Click_Event
     (Root_X, Root_Y : Integer;
      X, Y           : Integer)
      return UI_Event;

   function Button_Press_Event
     (Root_X, Root_Y : Integer;
      X, Y           : Integer)
     return UI_Event;

   function Button_Release_Event
     (Root_X, Root_Y : Integer;
      X, Y           : Integer)
     return UI_Event;

   function Mouse_Enter_Event
     return UI_Event;

   function Mouse_Leave_Event
     return UI_Event;

private

   type UI_Event (Event_Type : UI_Event_Type) is
      record
         X, Y           : Integer;
         Root_X, Root_Y : Integer;
         Button         : Mouse_Button;
      end record;

end Chaos.UI.Events;
