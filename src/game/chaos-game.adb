with Ada.Text_IO;

with Lith.Objects;

with Chaos.UI;

with Chaos.Expressions;
with Chaos.Parser;

with Chaos.Resources.Manager;
with Chaos.Resources.Tables;

with Chaos.Areas.Import;
with Chaos.Features;

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
      else
         if Game.Area.Has_Feature (Actor.Location) then
            declare
               Feature : constant Chaos.Features.Chaos_Feature :=
                           Game.Area.Feature (Actor.Location);
            begin
               if Feature.Has_Destination then
                  Game.Travel
                    (Destination_Area_Name     => Feature.Destination_Name,
                     Destination_Entrance_Name =>
                       Feature.Destination_Entrance_Name);
               end if;
            end;
         end if;
      end if;
   end Arrive;

   ------------------
   -- Check_Combat --
   ------------------

   procedure Check_Combat
     (Game : in out Chaos_Game_Record'Class)
   is
      Have_Hostiles : Boolean := False;

      function Hostile
        (Actor : Chaos.Actors.Chaos_Actor)
         return Boolean
      is (Actor.Creature.Hostile);

      procedure Update_Hostile
        (Actor : Chaos.Actors.Chaos_Actor);

      --------------------
      -- Update_Hostile --
      --------------------

      procedure Update_Hostile
        (Actor : Chaos.Actors.Chaos_Actor)
      is
      begin
         if Actor.Creature.Hostile then
            Have_Hostiles := True;
         end if;
      end Update_Hostile;

   begin
      Game.Area.Scan_Matching_Actors
        (Hostile'Access, Update_Hostile'Access);

      if Game.In_Combat then
         if not Have_Hostiles then
            Game.Area.Log ("end of combat");
         end if;
      else
         if Have_Hostiles then
            Game.Area.Log ("combat start");
         end if;
      end if;

      Game.In_Combat := Have_Hostiles;
   end Check_Combat;

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
      if not Chaos.Locations.Adjacent (Actor.Location, Target.Location) then
         Actor.Update (Move'Access);
      else
         Game.Arrive (Actor);
      end if;
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

   -----------------
   -- Script_Flag --
   -----------------

   function Script_Flag
     (Game  : in out Chaos_Game_Record'Class;
      Actor : not null access constant
        Chaos.Objects.Root_Chaos_Object_Record'Class;
      Group : String;
      Name  : String)
      return Boolean
   is
      Key : constant String :=
              Actor.Identifier
              & "-" & Group
              & "-" & Name;
   begin
      return Game.Script_Flags.Contains (Key)
        and then Game.Script_Flags.Element (Key);
   end Script_Flag;

   ------------------
   -- Script_Round --
   ------------------

   procedure Script_Round (Game : in out Chaos_Game_Record'Class) is
   begin

      Game.Check_Combat;

      Game.Script_Flags.Clear;
      Game.Area.Execute_Script;
      for I in 1 .. Game.Area.Actor_Count loop
         Game.Area.Actor (I).Execute_Script;
      end loop;
   end Script_Round;

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

   ---------------------
   -- Set_Script_Flag --
   ---------------------

   procedure Set_Script_Flag
     (Game  : in out Chaos_Game_Record'Class;
      Actor : not null access constant
        Chaos.Objects.Root_Chaos_Object_Record'Class;
      Group : String;
      Name  : String)
   is
      Key : constant String :=
              Actor.Identifier
              & "-" & Group
              & "-" & Name;
   begin
      if Game.Script_Flags.Contains (Key) then
         Game.Script_Flags.Replace (Key, True);
      else
         Game.Script_Flags.Insert (Key, True);
      end if;
   end Set_Script_Flag;

   -----------------------
   -- Show_Dialog_State --
   -----------------------

   procedure Show_Dialog_State
     (Game : in out Chaos_Game_Record'Class)
   is
      use Chaos.Dialog;
      UI : constant Chaos.UI.Chaos_UI :=
             Chaos.UI.Current_UI;
   begin
      if Has_State (Game.Dialog_State) then
         UI.Put_Line (Text (Game.Dialog_State));

         if not Is_Finished (Game.Dialog_State) then
            if Choice_Count (Game.Dialog_State) = 1
              and then not Choice_Has_Text (Game.Dialog_State, 1)
            then
               Choose (Game.Dialog_State, 1);
            else
               for I in 1 .. Chaos.Dialog.Choice_Count (Game.Dialog_State) loop
                  UI.Put_Line
                    (Positive'Image (I) & ". "
                     & Chaos.Dialog.Choice_Text (Game.Dialog_State, I));
               end loop;
            end if;
         end if;
      end if;
   end Show_Dialog_State;

   -----------
   -- Start --
   -----------

   procedure Start (Game : in out Chaos_Game_Record'Class) is
      pragma Unreferenced (Game);
      Start : constant Lith.Objects.Object :=
                Chaos.Parser.Load_Script
                  (Chaos.Paths.Config_File ("start.script"));
   begin
      Chaos.Expressions.Store.Reset;
      Chaos.Expressions.Store.Report_State;
      Ada.Text_IO.Put_Line
        (Chaos.Expressions.Store.Show (Start));
      Ada.Text_IO.Put_Line
        (Chaos.Expressions.Store.Show
           (Chaos.Expressions.Store.Evaluate
                (Start)));
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
      Creature : Chaos.Creatures.Chaos_Creature;

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
         Creature := Talker.Creature;
      elsif Listener.Creature.Has_Dialog then
         Dialog := Listener.Creature.Dialog;
         Creature := Listener.Creature;
      else
         Chaos.UI.Current_UI.Activate (Listener);
         Chaos.UI.Current_UI.Display_Localised_Text
           ("no-dialog");
         Chaos.UI.Current_UI.Deactivate (Listener);
         return;
      end if;

      Chaos.Creatures.On_Start_Dialog
        (Talker.Creature, Listener.Creature);

      Game.Dialog := Dialog;
      Game.Dialog_State := Dialog.Start (Creature);

      Talker.Update (Update_Talker_Orientation'Access);
      Listener.Update (Update_Listener_Orientation'Access);

      Ada.Text_IO.Put_Line ("Starting dialog: " & Dialog.Identifier);

      Game.Show_Dialog_State;

      if not Chaos.Dialog.Has_State (Game.Dialog_State) then
         Chaos.UI.Current_UI.Put_Line
           (Listener.Display_Name
            & " has nothing to say to you");
      end if;

   end Start_Dialog;

   ------------
   -- Travel --
   ------------

   procedure Travel
     (Game                      : in out Chaos_Game_Record'Class;
      Destination_Area_Name     : String;
      Destination_Entrance_Name : String)
   is
      use type Chaos.Actors.Chaos_Actor;
   begin

      for I in Chaos.Party.Party_Member_Index loop
         exit when Game.Party.Party_Member (I) = null;
         Game.Area.Remove_Actor (Game.Party.Party_Member (I));
      end loop;

      if Chaos.Areas.Exists (Destination_Area_Name) then
         Game.Area := Chaos.Areas.Get (Destination_Area_Name);
      else
         Game.Area := Chaos.Areas.Import.Import_Area (Destination_Area_Name);
      end if;

      declare
         procedure Update_Location
           (Actor : in out Chaos.Actors.Chaos_Actor_Record'Class);

         ---------------------
         -- Update_Location --
         ---------------------

         procedure Update_Location
           (Actor : in out Chaos.Actors.Chaos_Actor_Record'Class)
         is
         begin
            Actor.Set_Initial_Location
              (Game.Area.Entrance_Square (Destination_Entrance_Name));
         end Update_Location;

      begin
         Game.Party.Party_Member (1).Update (Update_Location'Access);
      end;

      Game.Area.Add_Actor (Game.Party.Party_Member (1));

      Chaos.UI.Current_UI.Show_Area (Game.Area);

   end Travel;

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
