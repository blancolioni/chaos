package body Chaos.UI is

   Local_Current_UI : Chaos_UI;

   ----------------
   -- Current_UI --
   ----------------

   function Current_UI return Chaos_UI is
   begin
      return Local_Current_UI;
   end Current_UI;

   ----------------------------
   -- Display_Localised_Text --
   ----------------------------

   procedure Display_Localised_Text
     (UI    : in out Root_Chaos_UI'Class;
      Tag   : String)
   is
   begin
      UI.Display_Text (Chaos.Localisation.Local_Text (Tag));
   end Display_Localised_Text;

   ----------------------------
   -- Display_Localised_Text --
   ----------------------------

   procedure Display_Localised_Text
     (UI    : in out Root_Chaos_UI'Class;
      Index : Chaos.Localisation.Local_Text_Index)
   is
   begin
      UI.Display_Text (Chaos.Localisation.Indexed_Text (Index));
   end Display_Localised_Text;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (UI     : in out Root_Chaos_UI'Class)
   is
      pragma Unreferenced (UI);
   begin
      null;
   end Initialize;

   --------------------
   -- Set_Current_UI --
   --------------------

   procedure Set_Current_UI
     (UI : Chaos_UI)
   is
   begin
      Local_Current_UI := UI;
      Chaos.Animations.Set_Animation_Factory (UI);
   end Set_Current_UI;

end Chaos.UI;
