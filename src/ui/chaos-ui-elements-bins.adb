package body Chaos.UI.Elements.Bins is

   ---------
   -- Add --
   ---------

   overriding procedure Add
     (Container : not null access Root_UI_Bin;
      Item      : not null access Chaos.UI.Elements.Root_UI_Element'Class)
   is
   begin
      Container.Clear;
      Chaos.UI.Elements.Containers.Root_UI_Container
        (Container.all).Add (Item);
   end Add;

   -------------
   -- New_Bin --
   -------------

   procedure New_Bin
     (Bin   : in out Root_UI_Bin'Class;
      Id    : String;
      Child : access Root_UI_Element'Class)
   is
   begin
      Bin.Create (Id);
      Bin.Set_Child (UI_Element (Child));
   end New_Bin;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
     (Bin   : in out Root_UI_Bin;
      Child : UI_Element)
   is
   begin
      Root_UI_Bin'Class (Bin).Add (Child);
   end Set_Child;

end Chaos.UI.Elements.Bins;
