package body Chaos.UI.Main is

   Local_Current_UI : Chaos_UI;

   ----------------
   -- Current_UI --
   ----------------

   function Current_UI return Chaos_UI is
   begin
      return Local_Current_UI;
   end Current_UI;

   ----------
   -- Init --
   ----------

   procedure Init (UI : not null access Root_Chaos_UI'Class) is
   begin
      Local_Current_UI := Chaos_UI (UI);
   end Init;

   ----------------
   -- Invalidate --
   ----------------

   procedure Invalidate
     (UI             : in out Root_Chaos_UI'Class;
      Layout_Changed : Boolean := False)
   is
   begin
      UI.Invalidate (0, 0, 1024, 768, Layout_Changed);
   end Invalidate;

end Chaos.UI.Main;
