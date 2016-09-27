package body Chaos.UI.Events is

   procedure Init_Event
     (Event          : in out UI_Event;
      Root_X, Root_Y : Integer;
      X, Y           : Integer);

   ------------------------
   -- Button_Press_Event --
   ------------------------

   function Button_Press_Event
     (Root_X, Root_Y : Integer;
      X, Y           : Integer)
      return UI_Event
   is
      Event : UI_Event (Button_Press);
   begin
      Init_Event (Event, Root_X, Root_Y, X, Y);
      return Event;
   end Button_Press_Event;

   --------------------------
   -- Button_Release_Event --
   --------------------------

   function Button_Release_Event
     (Root_X, Root_Y : Integer;
      X, Y           : Integer)
      return UI_Event
   is
      Event : UI_Event (Button_Release);
   begin
      Init_Event (Event, Root_X, Root_Y, X, Y);
      return Event;
   end Button_Release_Event;

   -----------------
   -- Click_Event --
   -----------------

   function Click_Event
     (Root_X, Root_Y : Integer;
      X, Y           : Integer)
      return UI_Event
   is
      Event : UI_Event (Click);
   begin
      Init_Event (Event, Root_X, Root_Y, X, Y);
      return Event;
   end Click_Event;

   ----------------
   -- Init_Event --
   ----------------

   procedure Init_Event
     (Event          : in out UI_Event;
      Root_X, Root_Y : Integer;
      X, Y           : Integer)
   is
   begin
      Event.Root_X := Root_X;
      Event.Root_Y := Root_Y;
      Event.X := X;
      Event.Y := Y;
   end Init_Event;

   -----------------------
   -- Mouse_Enter_Event --
   -----------------------

   function Mouse_Enter_Event
      return UI_Event
   is
      Event : UI_Event (Mouse_Enter);
   begin
      Init_Event (Event, 0, 0, 0, 0);
      return Event;
   end Mouse_Enter_Event;

   -----------------------
   -- Mouse_Leave_Event --
   -----------------------

   function Mouse_Leave_Event
      return UI_Event
   is
      Event : UI_Event (Mouse_Leave);
   begin
      Init_Event (Event, 0, 0, 0, 0);
      return Event;
   end Mouse_Leave_Event;

end Chaos.UI.Events;
