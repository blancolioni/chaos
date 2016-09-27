with Chaos.Actors;
with Chaos.Battles;
with Chaos.Locations;
with Chaos.Players;

package Chaos.UI is

   type Root_UI_Model is abstract tagged private;

   procedure On_Encounter_Start
     (Model    : in out Root_UI_Model)
   is null;

   procedure On_Encounter_End
     (Model    : in out Root_UI_Model)
   is null;

   procedure Start_Dialog
     (Model    : in out Root_UI_Model;
      Actor    : Chaos.Actors.Chaos_Actor;
      Target   : Chaos.Actors.Chaos_Actor)
   is null;

   procedure On_Creature_Death
     (Model    : Root_UI_Model;
      Creature : Chaos.Actors.Chaos_Actor)
   is null;

   procedure Creature_Walk
     (Model    : in out Root_UI_Model;
      Creature : Chaos.Actors.Chaos_Actor;
      Path     : Chaos.Locations.Square_Path)
   is null;

   procedure Active_Creature
     (Model : in out Root_UI_Model;
      Creature : Chaos.Actors.Chaos_Actor)
   is null;

   procedure Creature_End_Turn
     (Model : in out Root_UI_Model;
      Creature : Chaos.Actors.Chaos_Actor)
   is null;

   procedure Creature_Wait
     (Model : in out Root_UI_Model;
      Creature : Chaos.Actors.Chaos_Actor)
   is null;

   type UI_Model is access all Root_UI_Model'Class;

   function Current_Model return UI_Model;

   type Root_Chaos_UI is abstract new Chaos.Battles.Battle_Manager_Interface
   with private;

   overriding function Active_Battle
     (UI : Root_Chaos_UI)
      return Boolean
   is (False);

   procedure Start (UI : in out Root_Chaos_UI) is abstract;

   procedure Stop (UI : in out Root_Chaos_UI) is abstract;

   procedure Show_Model
     (UI    : in out Root_Chaos_UI;
      Model : not null access Root_UI_Model'Class)
   is abstract;

   function Party
     (Model : Root_Chaos_UI'Class)
      return Chaos.Players.Party_Type;

   type Chaos_UI is access all Root_Chaos_UI'Class;

   function Current_UI return Chaos_UI;

private

   type Root_UI_Model is abstract tagged null record;

   type Root_Chaos_UI is abstract new
     Chaos.Battles.Battle_Manager_Interface with
      record
         Party : Chaos.Players.Party_Type :=
                   new Chaos.Players.Root_Party_Type;
      end record;

   Local_Current_UI    : Chaos_UI;
   Local_Current_Model : UI_Model;

end Chaos.UI;
