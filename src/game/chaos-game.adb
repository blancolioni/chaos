with Ada.Text_IO;

with Chaos.UI;

with Chaos.Expressions;
with Chaos.Parser;

with Chaos.Paths;

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

   ------------
   -- Arrive --
   ------------

   procedure Arrive
     (Game        : in out Chaos_Game_Record'Class;
      Actor       : Chaos.Actors.Chaos_Actor)
   is
   begin
      case Game.Interaction is
         when Default =>
            Game.Start_Dialog (Actor, Game.Target);
         when Talk =>
            Game.Start_Dialog (Actor, Game.Target);
         when Attack =>
            null;
         when Steal =>
            null;
         when Disarm =>
            null;
         when Manipulate =>
            null;
      end case;
   end Arrive;

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

   --------------
   -- Interact --
   --------------

   procedure Interact
     (Game        : in out Chaos_Game_Record'Class;
      Actor       : Chaos.Actors.Chaos_Actor;
      Target      : Chaos.Actors.Chaos_Actor;
      Interaction : Interaction_Type := Default)
   is
      procedure Move
        (Mover : in out Chaos.Actors.Chaos_Actor_Record'Class);

      ----------
      -- Move --
      ----------

      procedure Move
        (Mover : in out Chaos.Actors.Chaos_Actor_Record'Class)
      is
      begin
         Mover.Set_Orientation
           (Chaos.Locations.Get_Direction
              (Mover.Location, Target.Location));
         if not Chaos.Locations.Adjacent
           (Mover.Location, Target.Location)
         then
            Mover.Set_Path
              (Chaos.Locations.Drop_Last
                 (Game.Area.Find_Path
                      (Mover.Location, Target.Location)));
         end if;
      end Move;

   begin
      Game.Interaction := Interaction;
      Game.Target := Target;
      Actor.Update (Move'Access);
   end Interact;

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

   -------------------
   -- Select_Option --
   -------------------

   procedure Select_Option
     (Game   : in out Chaos_Game_Record'Class;
      Option : Positive)
   is
      use Chaos.Dialog;
   begin
      if Game.Dialog /= null
        and then Option <= Choice_Count (Game.Dialog_State)
      then
         Choose (Game.Dialog_State, Option);
         Game.Show_Dialog_State;
      end if;
   end Select_Option;

   -----------------------
   -- Show_Dialog_State --
   -----------------------

   procedure Show_Dialog_State
     (Game : in out Chaos_Game_Record'Class)
   is
      UI : constant Chaos.UI.Chaos_UI :=
             Chaos.UI.Current_UI;
   begin
      UI.Display_Text (Chaos.Dialog.Text (Game.Dialog_State));
      if Chaos.Dialog.Choice_Count (Game.Dialog_State) = 0 then
         Game.Dialog := null;
      else
         for I in 1 .. Chaos.Dialog.Choice_Count (Game.Dialog_State) loop
            UI.Display_Text
              (Positive'Image (I) & ". "
               & Chaos.Dialog.Choice_Text (Game.Dialog_State, I));
         end loop;
      end if;
   end Show_Dialog_State;

   -----------
   -- Start --
   -----------

   procedure Start (Game : in out Chaos_Game_Record'Class) is
      pragma Unreferenced (Game);
   begin
      Ada.Text_IO.Put_Line
        (Chaos.Expressions.To_String
           (Chaos.Parser.Load_Script
                (Chaos.Paths.Config_File ("start.script"))));
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

      Game.Show_Dialog_State;

   end Start_Dialog;

end Chaos.Game;
