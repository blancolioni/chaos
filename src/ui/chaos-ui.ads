with Chaos.Actors;
with Chaos.Battles;

package Chaos.UI is

   type Root_Chaos_UI is abstract new Chaos.Battles.Battle_Manager_Interface
   with private;

   overriding function Active_Battle
     (UI : Root_Chaos_UI)
      return Boolean
   is (False);

   procedure Start (UI : in out Root_Chaos_UI) is abstract;

   procedure Stop (UI : in out Root_Chaos_UI) is abstract;

   procedure Display_Text
     (UI    : in out Root_Chaos_UI;
      Text  : String)
   is abstract;

   procedure Display_Localised_Text
     (UI    : in out Root_Chaos_UI'Class;
      Tag   : String);

   procedure Activate
     (UI    : in out Root_Chaos_UI;
      Actor : Chaos.Actors.Chaos_Actor)
   is null;

   procedure Deactivate
     (UI    : in out Root_Chaos_UI;
      Actor : Chaos.Actors.Chaos_Actor)
   is null;

   type Chaos_UI is access all Root_Chaos_UI'Class;

   function Current_UI return Chaos_UI;

private

   type Root_UI_Model is abstract tagged null record;

   type Root_Chaos_UI is abstract new
     Chaos.Battles.Battle_Manager_Interface with
      record
         null;
      end record;

   Local_Current_UI    : Chaos_UI;

end Chaos.UI;
