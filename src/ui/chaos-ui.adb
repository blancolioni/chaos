package body Chaos.UI is

   -------------------
   -- Current_Model --
   -------------------

   function Current_Model return UI_Model is
   begin
      return Local_Current_Model;
   end Current_Model;

   ----------------
   -- Current_UI --
   ----------------

   function Current_UI return Chaos_UI is
   begin
      return Local_Current_UI;
   end Current_UI;

   -----------
   -- Party --
   -----------

   function Party
     (Model : Root_Chaos_UI'Class)
      return Chaos.Players.Party_Type
   is
   begin
      return Model.Party;
   end Party;

end Chaos.UI;
