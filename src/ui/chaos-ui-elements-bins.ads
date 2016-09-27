with Chaos.UI.Elements.Containers;

package Chaos.UI.Elements.Bins is

   type Root_UI_Bin is
     new Chaos.UI.Elements.Containers.Root_UI_Container with private;

   procedure Set_Child
     (Bin   : in out Root_UI_Bin;
      Child : UI_Element);

   procedure New_Bin
     (Bin   : in out Root_UI_Bin'Class;
      Id    : String;
      Child : access Root_UI_Element'Class);

private

   type Root_UI_Bin is
     new Chaos.UI.Elements.Containers.Root_UI_Container with null record;

   overriding procedure Add
     (Container : not null access Root_UI_Bin;
      Item      : not null access Chaos.UI.Elements.Root_UI_Element'Class);

end Chaos.UI.Elements.Bins;
