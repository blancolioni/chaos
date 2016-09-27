with Chaos.UI.Models;

package Chaos.UI.Main is

   type Root_Chaos_UI is abstract tagged private;

   procedure Init (UI : not null access Root_Chaos_UI'Class);

   procedure Start (UI : in out Root_Chaos_UI) is abstract;

   procedure Stop (UI : in out Root_Chaos_UI) is abstract;

   procedure Invalidate
     (UI             : in out Root_Chaos_UI;
      X, Y           : Integer;
      Width, Height  : Natural;
      Layout_Changed : Boolean)
   is abstract;

   procedure Invalidate
     (UI             : in out Root_Chaos_UI'Class;
      Layout_Changed : Boolean := False);

   procedure Show_Model
     (UI    : in out Root_Chaos_UI;
      Model : not null access Chaos.UI.Models.Root_UI_Model'Class)
   is abstract;

   type Chaos_UI is access all Root_Chaos_UI'Class;

   function Current_UI return Chaos_UI;

private

   type Root_Chaos_UI is abstract tagged null record;

end Chaos.UI.Main;
