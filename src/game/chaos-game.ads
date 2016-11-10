private with WL.String_Maps;

with Chaos.Actors;
with Chaos.Areas;
with Chaos.Creatures;
with Chaos.Dialog;
with Chaos.Locations;
with Chaos.Objects;
with Chaos.Party;

package Chaos.Game is

   type Interaction_Type is
     (Default, Talk, Attack, Steal, Disarm, Manipulate);

   type Chaos_Game_Record is tagged private;

   function Party
     (Game : Chaos_Game_Record'Class)
      return Chaos.Party.Party_Type;

   function Area
     (Game : Chaos_Game_Record'Class)
      return Chaos.Areas.Chaos_Area;

   procedure Actor_End_Turn
     (Game  : in out Chaos_Game_Record'Class;
      Actor : Chaos.Actors.Chaos_Actor);

   procedure Actor_Wait
     (Game  : in out Chaos_Game_Record'Class;
      Actor : Chaos.Actors.Chaos_Actor);

   procedure Actor_Walk
     (Game  : in out Chaos_Game_Record'Class;
      Actor : Chaos.Actors.Chaos_Actor;
      Path  : Chaos.Locations.Square_Path);

   procedure Start_Dialog
     (Game     : in out Chaos_Game_Record'Class;
      Talker   : Chaos.Actors.Chaos_Actor;
      Listener : Chaos.Actors.Chaos_Actor);

   procedure Select_Option
     (Game   : in out Chaos_Game_Record'Class;
      Option : Positive);

   procedure Interact
     (Game        : in out Chaos_Game_Record'Class;
      Actor       : Chaos.Actors.Chaos_Actor;
      Target      : Chaos.Actors.Chaos_Actor;
      Interaction : Interaction_Type := Default);

   procedure Walk_To
     (Game        : in out Chaos_Game_Record'Class;
      Actor       : Chaos.Actors.Chaos_Actor;
      Destination : Chaos.Locations.Square_Location);

   procedure Travel
     (Game                      : in out Chaos_Game_Record'Class;
      Destination_Area_Name     : String;
      Destination_Entrance_Name : String);

   procedure Arrive
     (Game        : in out Chaos_Game_Record'Class;
      Actor       : Chaos.Actors.Chaos_Actor);

   procedure Start (Game : in out Chaos_Game_Record'Class);

   procedure Script_Round (Game : in out Chaos_Game_Record'Class);

   procedure Set_Script_Flag
     (Game  : in out Chaos_Game_Record'Class;
      Actor : not null access constant
        Chaos.Objects.Root_Chaos_Object_Record'Class;
      Group : String;
      Name  : String);

   function Script_Flag
     (Game  : in out Chaos_Game_Record'Class;
      Actor : not null access constant
        Chaos.Objects.Root_Chaos_Object_Record'Class;
      Group : String;
      Name  : String)
      return Boolean;

   type Chaos_Game is access all Chaos_Game_Record'Class;

   function Current_Game return Chaos_Game;

   procedure Create_Game
     (Protagonist : Chaos.Creatures.Chaos_Creature);

private

   package Script_Flag_Maps is
     new WL.String_Maps (Boolean);

   type Chaos_Game_Record is tagged
      record
         Area         : Chaos.Areas.Chaos_Area;
         Party        : Chaos.Party.Party_Type;
         Interaction  : Interaction_Type;
         Target       : Chaos.Actors.Chaos_Actor;
         Dialog       : Chaos.Dialog.Chaos_Dialog;
         Dialog_State : Chaos.Dialog.Dialog_Cursor;
         Script_Flags : Script_Flag_Maps.Map;
         In_Combat    : Boolean := False;
      end record;

   procedure Show_Dialog_State
     (Game : in out Chaos_Game_Record'Class);

   procedure Check_Combat
     (Game : in out Chaos_Game_Record'Class);

end Chaos.Game;
