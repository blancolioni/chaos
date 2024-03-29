with Ada.Text_IO;

with Chaos.Races;

with Chaos.Game;

with Chaos.Images;

with Chaos.Resources.Tis;

package body Chaos.UI.Text_UI is

   type Text_Image_Container is
     new Chaos.Images.Root_Chaos_Image_Container with null record;

   overriding procedure Import_Tileset
     (Container : in out Text_Image_Container;
      Tis       : Chaos.Resources.Tis.Tis_Resource'Class)
   is null;

   Local_Image_Container : aliased Text_Image_Container;

   type Chaos_Text_UI is
     new Root_Chaos_UI with null record;

   overriding procedure Start
     (UI : in out Chaos_Text_UI);

   overriding procedure Stop
     (UI : in out Chaos_Text_UI)
   is null;

   overriding procedure Put
     (UI : in out Chaos_Text_UI;
      Text : String);

   overriding function Create_Image_Container
     (UI : Chaos_Text_UI)
      return Chaos.Images.Chaos_Image_Container
   is (Local_Image_Container'Access);

   overriding function Create_Animation
     (UI : Chaos_Text_UI)
      return Chaos.Animations.Chaos_Animation
   is (null);

   overriding procedure Show_Area
     (UI       : in out Chaos_Text_UI;
      New_Area : Chaos.Areas.Chaos_Area)
   is null;

   ------------
   -- Create --
   ------------

   function Create return Chaos_UI is
   begin
      return new Chaos_Text_UI;
   end Create;

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (UI   : in out Chaos_Text_UI;
      Text : String)
   is
      pragma Unreferenced (UI);
   begin
      Ada.Text_IO.Put (Text);
   end Put;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (UI : in out Chaos_Text_UI)
   is
   begin
      Chaos_Text_UI'Class (UI).Initialize;
      UI.Put_Line ("Starting text interface ...");
      Chaos.Game.Current_Game.Start;
   end Start;

end Chaos.UI.Text_UI;
