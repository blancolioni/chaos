with Chaos.Actors;
with Chaos.Animations;
with Chaos.Battles;
with Chaos.Images;
with Chaos.Localisation;

package Chaos.UI is

   type Root_Chaos_UI is abstract new
     Chaos.Battles.Battle_Manager_Interface
     and Chaos.Animations.Chaos_Animation_Factory
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

   procedure Display_Localised_Text
     (UI    : in out Root_Chaos_UI'Class;
      Index : Chaos.Localisation.Local_Text_Index);

   procedure Activate
     (UI    : in out Root_Chaos_UI;
      Actor : Chaos.Actors.Chaos_Actor)
   is null;

   procedure Deactivate
     (UI    : in out Root_Chaos_UI;
      Actor : Chaos.Actors.Chaos_Actor)
   is null;

   procedure Initialize
     (UI     : in out Root_Chaos_UI'Class);

   function Create_Image_Container
     (UI : Root_Chaos_UI)
      return Chaos.Images.Chaos_Image_Container
      is abstract;

   overriding function Create_Animation
     (UI : Root_Chaos_UI)
      return Chaos.Animations.Chaos_Animation
      is abstract;

   type Chaos_UI is access all Root_Chaos_UI'Class;

   function Current_UI return Chaos_UI;

   procedure Set_Current_UI
     (UI : Chaos_UI);

private

   type Root_UI_Model is abstract tagged null record;

   type Root_Chaos_UI is abstract new
     Chaos.Battles.Battle_Manager_Interface
     and Chaos.Animations.Chaos_Animation_Factory with
      record
         null;
      end record;

end Chaos.UI;
