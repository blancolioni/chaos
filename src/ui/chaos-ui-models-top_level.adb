with Ada.Text_IO;

with Chaos.UI.Elements.Containers;
with Chaos.UI.Elements.Buttons;

with Chaos.UI.Main;

with Chaos.UI.Models.Character_Gen;

package body Chaos.UI.Models.Top_Level is

   type Root_Top_Level_Model is new Root_UI_Model with
      record
         Button_Box : Chaos.UI.Elements.UI_Element;
      end record;

   overriding function Top_Element
     (Model : Root_Top_Level_Model)
      return Chaos.UI.Elements.UI_Element;

   Single_Top_Level_Model : UI_Model;

   procedure Create_Model;

   procedure On_New_Game_Click
     (Button : not null access Chaos.UI.Elements.Buttons.Root_UI_Button'Class);

   procedure On_Exit_Game_Click
     (Button : not null access Chaos.UI.Elements.Buttons.Root_UI_Button'Class);

   ------------------
   -- Create_Model --
   ------------------

   procedure Create_Model is
      M         : Root_Top_Level_Model;
      Container : constant Chaos.UI.Elements.Containers.UI_Container :=
                    Chaos.UI.Elements.Containers.New_Container
                      (Id          => "top-level-button-box");
   begin
      M.Init_Model ("top-level");
      Container.Add (Chaos.UI.Elements.Buttons.New_Button
               ("new-game-button", "New Game", On_New_Game_Click'Access));
      Container.Add (Chaos.UI.Elements.Buttons.New_Button
               ("exit-game-button", "Exit Game", On_Exit_Game_Click'Access));
      M.Button_Box := Chaos.UI.Elements.UI_Element (Container);

      Single_Top_Level_Model := new Root_Top_Level_Model'(M);
      Container.Set_Parent (Single_Top_Level_Model);
      Single_Top_Level_Model.Load_Style_Rules;
      Single_Top_Level_Model.Load_Style_Rules ("hover");
      Single_Top_Level_Model.Load_Style_Rules ("active");
   end Create_Model;

   ------------------------
   -- On_Exit_Game_Click --
   ------------------------

   procedure On_Exit_Game_Click
     (Button : not null access Chaos.UI.Elements.Buttons.Root_UI_Button'Class)
   is
      pragma Unreferenced (Button);
   begin
      Chaos.UI.Main.Current_UI.Stop;
   end On_Exit_Game_Click;

   -----------------------
   -- On_New_Game_Click --
   -----------------------

   procedure On_New_Game_Click
     (Button : not null access Chaos.UI.Elements.Buttons.Root_UI_Button'Class)
   is
      pragma Unreferenced (Button);
   begin
      Ada.Text_IO.Put_Line ("New game!");
      Chaos.UI.Main.Current_UI.Show_Model
        (Chaos.UI.Models.Character_Gen.Character_Gen_Model);
   end On_New_Game_Click;

   -----------------
   -- Top_Element --
   -----------------

   overriding function Top_Element
     (Model : Root_Top_Level_Model)
      return Chaos.UI.Elements.UI_Element
   is
   begin
      return Model.Button_Box;
   end Top_Element;

   ---------------------
   -- Top_Level_Model --
   ---------------------

   function Top_Level_Model return UI_Model is
      use type Chaos.UI.Elements.UI_Element;
   begin
      if Single_Top_Level_Model = null then
         Create_Model;
      end if;
      return Single_Top_Level_Model;
   end Top_Level_Model;

end Chaos.UI.Models.Top_Level;
