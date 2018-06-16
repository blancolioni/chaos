with Ada.Text_IO;

with Lith.Objects;
with Lith.Objects.Interfaces;

with Chaos.Expressions;

package body Chaos.UI is

   Local_Current_UI : Chaos_UI;

   function Evaluate_UI_Display_String
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

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
      UI.Put (Chaos.Localisation.Local_Text (Tag));
   end Display_Localised_Text;

   ----------------------------
   -- Display_Localised_Text --
   ----------------------------

   procedure Display_Localised_Text
     (UI    : in out Root_Chaos_UI'Class;
      Index : Chaos.Localisation.Local_Text_Index)
   is
   begin
      UI.Put (Chaos.Localisation.Indexed_Text (Index));
   end Display_Localised_Text;

   --------------------------------
   -- Evaluate_UI_Display_String --
   --------------------------------

   function Evaluate_UI_Display_String
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
   begin
      Chaos.UI.Current_UI.Put
        (Chaos.Localisation.Indexed_Text
           (Chaos.Localisation.Local_Text_Index
                (Lith.Objects.To_Integer
                     (Store.Argument (1)))));
      return Lith.Objects.Undefined;
   end Evaluate_UI_Display_String;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (UI     : in out Root_Chaos_UI'Class)
   is
      pragma Unreferenced (UI);
   begin
      Lith.Objects.Interfaces.Define_Function
        (Chaos.Expressions.Store,
         "ui-display-string",
         Evaluate_UI_Display_String'Access);
   end Initialize;

   --------------
   -- New_Line --
   --------------

   procedure New_Line
     (UI       : in out Root_Chaos_UI'Class)
   is
   begin
      UI.Put ((1 => Character'Val (10)));
   end New_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (UI       : in out Root_Chaos_UI'Class;
      Text     : String)
   is
   begin
      UI.Put (Text);
      UI.New_Line;
      Ada.Text_IO.Put_Line (Text);
   end Put_Line;

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
