with Ada.Text_IO;

with Lith.Objects;

with Chaos.UI;

with Chaos.Expressions;
with Chaos.Parser;

with Chaos.Resources.Manager;
with Chaos.Resources.Tables;

with Chaos.Areas.Import;

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
      use type Chaos.Actors.Chaos_Actor;
   begin
      if Game.Target /= null then
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
      end if;
   end Arrive;

   -----------------
   -- Create_Game --
   -----------------

   procedure Create_Game
     (Protagonist : Chaos.Creatures.Chaos_Creature)
   is
      Start_Area : Chaos.Resources.Tables.Table_Resource'Class renames
                     Chaos.Resources.Tables.Table_Resource'Class
                       (Chaos.Resources.Manager.Load_Resource
                          ("STARTARE", Chaos.Resources.Table_Resource).all);
      Area_Resource_Name : constant String :=
                             Start_Area.Get ("START_AREA", "VALUE");
      Area_X             : constant Integer :=
                             Integer'Value
                               (Start_Area.Get ("START_XPOS", "VALUE"));
      Area_Y             : constant Integer :=
                             Integer'Value
                               (Start_Area.Get ("START_YPOS", "VALUE"));
   begin
      Local_Current_Game := new Chaos_Game_Record;
      Local_Current_Game.Area :=
        Chaos.Areas.Import.Import_Area (Area_Resource_Name);
      Local_Current_Game.Party := Chaos.Party.Create_Party;
      Local_Current_Game.Party.Add_Party_Member
        (Chaos.Actors.Create_Actor
           (Protagonist, Local_Current_Game.Area,
            Local_Current_Game.Area.To_Square ((Area_X, Area_Y + 128)),
            Chaos.Locations.South));
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
      UI.Put_Line (Chaos.Dialog.Text (Game.Dialog_State));
      if Chaos.Dialog.Choice_Count (Game.Dialog_State) = 0 then
         Game.Dialog := null;
      else
         for I in 1 .. Chaos.Dialog.Choice_Count (Game.Dialog_State) loop
            UI.Put_Line
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
        (Chaos.Expressions.Store.Show
           (Chaos.Expressions.Store.Evaluate
                (Chaos.Parser.Load_Script
                     (Chaos.Paths.Config_File ("start.script"),
                      Chaos.Expressions.Store.all),
                 Lith.Objects.Nil)));
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

      procedure Update_Listener_Orientation
        (Actor : in out Chaos.Actors.Chaos_Actor_Record'Class);

      procedure Update_Talker_Orientation
        (Actor : in out Chaos.Actors.Chaos_Actor_Record'Class);

      ---------------------------------
      -- Update_Listener_Orientation --
      ---------------------------------

      procedure Update_Listener_Orientation
        (Actor : in out Chaos.Actors.Chaos_Actor_Record'Class)
      is
      begin
         Actor.Set_Orientation
           (Chaos.Locations.Get_Direction
              (Actor.Location, Talker.Location));
      end Update_Listener_Orientation;

      -------------------------------
      -- Update_Talker_Orientation --
      -------------------------------

      procedure Update_Talker_Orientation
        (Actor : in out Chaos.Actors.Chaos_Actor_Record'Class)
      is
      begin
         Actor.Set_Orientation
           (Chaos.Locations.Get_Direction
              (Actor.Location, Listener.Location));
      end Update_Talker_Orientation;

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

      Talker.Update (Update_Talker_Orientation'Access);
      Listener.Update (Update_Listener_Orientation'Access);

      Game.Show_Dialog_State;

   end Start_Dialog;

   -------------
   -- Walk_To --
   -------------

   procedure Walk_To
     (Game        : in out Chaos_Game_Record'Class;
      Actor       : Chaos.Actors.Chaos_Actor;
      Destination : Chaos.Locations.Square_Location)
   is
      use Chaos.Locations;

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
              (Mover.Location, Destination));
         Mover.Set_Path
           (Game.Area.Find_Path
              (Mover.Location, Destination));
      end Move;

   begin
      if Destination /= Actor.Location then
         Game.Interaction := Default;
         Game.Target := null;
         Actor.Update (Move'Access);
      end if;
   end Walk_To;

end Chaos.Game;
