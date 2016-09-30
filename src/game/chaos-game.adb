with Chaos.UI;

with Chaos.Expressions.Environments;

package body Chaos.Game is

   Local_Current_Game : Chaos_Game;

   --------------------
   -- Actor_End_Turn --
   --------------------

   procedure Actor_End_Turn
     (Game  : in out Chaos_Game_Record'Class;
      Actor : Chaos.Actors.Chaos_Actor)
   is
      pragma Unreferenced (Game);
      pragma Unreferenced (Actor);
   begin
      null;
   end Actor_End_Turn;

   ----------------
   -- Actor_Wait --
   ----------------

   procedure Actor_Wait
     (Game  : in out Chaos_Game_Record'Class;
      Actor : Chaos.Actors.Chaos_Actor)
   is
      pragma Unreferenced (Game);
      pragma Unreferenced (Actor);
   begin
      null;
   end Actor_Wait;

   ----------------
   -- Actor_Walk --
   ----------------

   procedure Actor_Walk
     (Game  : in out Chaos_Game_Record'Class;
      Actor : Chaos.Actors.Chaos_Actor;
      Path  : Chaos.Locations.Square_Path)
   is
      pragma Unreferenced (Game);
      pragma Unreferenced (Actor);
   begin
      null;
   end Actor_Walk;

   ----------
   -- Area --
   ----------

   function Area
     (Game : Chaos_Game_Record'Class)
      return Chaos.Areas.Chaos_Area
   is
   begin
      return Game.Area;
   end Area;

   -----------------
   -- Create_Game --
   -----------------

   procedure Create_Game
     (Area   : Chaos.Areas.Chaos_Area;
      Party  : Chaos.Party.Party_Type)
   is
   begin
      Local_Current_Game := new Chaos_Game_Record;
      Local_Current_Game.Area := Area;
      Local_Current_Game.Party := Party;
   end Create_Game;

   ------------------
   -- Current_Game --
   ------------------

   function Current_Game return Chaos_Game is
   begin
      return Local_Current_Game;
   end Current_Game;

   -----------
   -- Party --
   -----------

   function Party
     (Game : Chaos_Game_Record'Class)
      return Chaos.Party.Party_Type
   is
   begin
      return Game.Party;
   end Party;

   -----------
   -- Start --
   -----------

   procedure Start (Game : in out Chaos_Game_Record'Class) is
   begin
      Chaos.Expressions.Execute
        (Chaos.Expressions.Environments.Standard_Environment,
         Game.Area.Script);
   end Start;

   ------------------
   -- Start_Dialog --
   ------------------

   procedure Start_Dialog
     (Game     : in out Chaos_Game_Record'Class;
      Talker   : Chaos.Actors.Chaos_Actor;
      Listener : Chaos.Actors.Chaos_Actor)
   is
      Dialog : Chaos.Dialog.Chaos_Dialog;
   begin
      if Talker.Creature.Has_Dialog then
         Dialog := Talker.Creature.Dialog;
      elsif Listener.Creature.Has_Dialog then
         Dialog := Listener.Creature.Dialog;
      else
         Chaos.UI.Current_UI.Activate (Listener);
         Chaos.UI.Current_UI.Display_Localised_Text
           ("no-dialog");
         Chaos.UI.Current_UI.Deactivate (Listener);
         return;
      end if;

      Game.Dialog := Dialog;
      Game.Dialog_State := Dialog.Start;

   end Start_Dialog;

end Chaos.Game;
