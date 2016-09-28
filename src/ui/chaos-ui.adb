with Chaos.Localisation;

package body Chaos.UI is

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

end Chaos.UI;
