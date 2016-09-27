with Ada.Exceptions;
with Ada.Text_IO;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Numerics;

with Glib;
with Glib.Main;

with Gdk.Event;
with Gdk.RGBA;
with Gdk.Window;

with Gtk.Enums;

with Gtk.Box;
with Gtk.Button;
with Gtk.Drawing_Area;
with Gtk.Text_Buffer;
with Gtk.Text_Iter;
with Gtk.Text_Mark;
with Gtk.Text_View;
with Gtk.Widget;

with Cairo;
with Cairo.Matrix;

with Chaos.Actors;
with Chaos.Players;

with Chaos.Creatures.Encounters;
with Chaos.Creatures.Graphics;
with Chaos.Creatures.Update;

with Chaos.Players;

with Chaos.Powers;

with Chaos.Resources.Animations;
with Chaos.Resources.Bam.Cairo_Bam;
with Chaos.Resources.Dialog;

with Chaos.World.Map;
with Chaos.World.Updates;

with Chaos.UI.Logging;
with Chaos.UI.Gtk_UI.Resources;
with Chaos.UI.Screens;

with Chaos.Localisation;

with Chaos.Mutexes;

package body Chaos.UI.Gtk_UI.Main_Model is

   use Glib;

   DX : constant array (Chaos.Db.Direction) of Glib.Gdouble :=
          (0.0, 1.0, 1.0, 1.0, 0.0, -1.0, -1.0, -1.0);
   DY : constant array (Chaos.Db.Direction) of Glib.Gdouble :=
          (-1.0, -1.0, 0.0, 1.0, 1.0, 1.0, 0.0, -1.0);

   --  View transform
   Translate_X : constant := 0.67;
   Translate_Y : constant := 0.33;
   Scale_X     : constant := 1.0;
   Scale_Y     : constant := 0.75;
   Rotate_X    : constant := 0.5;
   Rotate_Y    : constant := 0.0;

   --  Direction D to animation mapping index A.
   --  If negative, reverse the image before rendering it using abs A - 1
   --  Otherwise, render using A - 1

   Direction_To_Animation : constant array (Chaos.Db.Direction) of Integer :=
                              (Chaos.Db.South_West => 2,
                               Chaos.Db.West       => 3,
                               Chaos.Db.North_West => 4,
                               Chaos.Db.North      => 5,
                               Chaos.Db.North_East => -4,
                               Chaos.Db.East       => -3,
                               Chaos.Db.South_East => -2,
                               Chaos.Db.South      => 1);

   Standing_Offset : constant := 7;
   Walking_Offset  : constant := -1;

   Animation_Update_Cycles : constant := 4;

   type Point is
      record
         X, Y : Glib.Gdouble;
      end record;

   type Quadrangle is array (1 .. 4) of Point;

   type Active_Creature_Record is
      record
         Animation       : Chaos.Resources.Bam.Bam_Resource;
         Animation_Index : Natural;
      end record;

   package List_Of_Creatures is
     new Ada.Containers.Doubly_Linked_Lists
       (Chaos.Db.Creature_Reference, Chaos.Db."=");

   package Active_Creature_Maps is
     new Chaos.Db.Creature_Maps (Active_Creature_Record);

   function Acts_Earlier
     (Left, Right : Chaos.Db.Creature_Reference)
      return Boolean;

   package Initiative_Sorting is
     new List_Of_Creatures.Generic_Sorting (Acts_Earlier);
   pragma Unreferenced (Initiative_Sorting);

   type Command_Button is
     new Gtk.Button.Gtk_Button_Record with
      record
         Command : Chaos.Players.Command_Type;
      end record;

   type Command_Button_Access is access all Command_Button'Class;

   function Create_Command_Button
     (Command : Chaos.Players.Command_Type)
      return Gtk.Button.Gtk_Button;

   procedure On_Command_Button_Click
     (Command_Button : access Gtk.Button.Gtk_Button_Record'Class);

   procedure Update_Command_Buttons;

   type Text_Buffer_Logger is
     new Chaos.UI.Logging.Root_Logger with
      record
         Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer;
         View   : Gtk.Text_View.Gtk_Text_View;
      end record;

   overriding procedure Log
     (Logger : Text_Buffer_Logger;
      Actor  : String;
      Message : String);

   type Active_Power_Attack_Type is
      record
         Attacker  : Chaos.Db.Creature_Reference;
         Defender  : Chaos.Db.Creature_Reference;
         Power     : Chaos.Db.Power_Reference;
      end record;

   type Root_Main_Model is
     new Root_Gtk_Model with
      record
         Draw_Area              : Gtk.Drawing_Area.Gtk_Drawing_Area;
         Feedback_Text_View     : Gtk.Text_View.Gtk_Text_View;
         Tool_Button_Box        : Gtk.Box.Gtk_Box;
         Width, Height          : Glib.Gdouble;
         Ground_Surface         : Cairo.Cairo_Surface := Cairo.Null_Surface;
         Creature_Surface       : Cairo.Cairo_Surface := Cairo.Null_Surface;
         Feedback_Surface       : Cairo.Cairo_Surface := Cairo.Null_Surface;
         Interface_Surface      : Cairo.Cairo_Surface := Cairo.Null_Surface;
         Animation_Surface      : Cairo.Cairo_Surface := Cairo.Null_Surface;
         Zoom                   : Positive := 32;
         Creature_List          : List_Of_Creatures.List;
         Creature_List_Mutex    : access Chaos.Mutexes.Mutex;
         Party_List             : List_Of_Creatures.List;
         Active_Creatures       : Active_Creature_Maps.Map;
         Centre_X, Centre_Y     : Chaos.World.World_Coordinate;
         Left_X, Top_Y          : Chaos.World.World_Coordinate;
         Right_X, Bottom_Y      : Chaos.World.World_Coordinate;
         Squares_Across         : Chaos.World.World_Coordinate;
         Squares_Down           : Chaos.World.World_Coordinate;
         Selected_Square        : Chaos.Areas.Area_Square_Location;
         Hover_Square           : Chaos.Areas.Area_Square_Location;
         Selected_Player        : Chaos.Players.Player_Type;
         Acting_Creature        : List_Of_Creatures.Cursor :=
                                    List_Of_Creatures.No_Element;
         Walking_Creature       : Chaos.Db.Creature_Reference :=
                                    Chaos.Db.Null_Creature_Reference;
         Walking_Path_Index     : Natural;
         Walking_Path           : access Chaos.Areas.Area_Square_Path;
         Walking_Partial        : Point;
         Ranged_Power           : Boolean := False;
         Power_Reference        : Chaos.Db.Power_Reference;
         Power_Surface          : Cairo.Cairo_Surface;
         Power_Start            : Point;
         Power_End              : Point;
         Power_Steps            : Natural;
         Power_Current_Step     : Natural;
         Current_Command        : Chaos.Players.Command_Type;
         Executing_Command      : Chaos.Players.Command_Type;
         Current_Animation      : Chaos.Resources.Animations.Animation_Type;
         Animation_Changed      : Boolean := False;
         Action_Timeout_Id      : Glib.Main.G_Source_Id;
         Creatures_Changed      : Boolean := False;
         Animation_Frame_Index  : Natural := 0;
         Current_Attack         : Active_Power_Attack_Type;
         Current_Dialog         : Chaos.Resources.Dialog.Dialog_Resource;
         Current_Dialog_State   : Natural := 0;
         Current_DX, Current_DY : Chaos.World.World_Coordinate := 0;
      end record;

   overriding procedure On_Creature_Death
     (Model    : Root_Main_Model;
      Creature : Chaos.Db.Creature_Reference);

   overriding procedure Creature_Walk
     (Model    : in out Root_Main_Model;
      Creature : Chaos.Db.Creature_Reference;
      Path     : Chaos.Areas.Area_Square_Path);

   overriding procedure Active_Creature
     (Model : in out Root_Main_Model;
      Creature : Chaos.Db.Creature_Reference);

   overriding procedure Creature_End_Turn
     (Model    : in out Root_Main_Model;
      Creature : Chaos.Db.Creature_Reference);

   overriding procedure Creature_Wait
     (Model    : in out Root_Main_Model;
      Creature : Chaos.Db.Creature_Reference);

   overriding procedure Start_Dialog
     (Model    : in out Root_Main_Model;
      Actor    : Chaos.Db.Creature_Reference;
      Target   : Chaos.Db.Creature_Reference);

   procedure Remove_Creature
     (Model : in out Root_Main_Model'Class;
      Creature : Chaos.Db.Creature.Creature_Type) with Unreferenced;

   type Main_Model_Access is access all Root_Main_Model'Class;

   Local_Main_Model : Main_Model_Access;

   procedure Create_Model;

   function On_Action_Timeout return Boolean;

   function On_Screen
     (X, Y : Integer)
      return Boolean
   is (X in Integer (Local_Main_Model.Left_X)
       .. Integer (Local_Main_Model.Right_X)
       and then Y in Integer (Local_Main_Model.Top_Y)
       .. Integer (Local_Main_Model.Bottom_Y));

   function Get_Square_Boundary
     (World_X, World_Y : Chaos.World.World_Coordinate)
      return Quadrangle;

   function Get_Square_Top_Left
     (World_X, World_Y : Chaos.World.World_Coordinate)
      return Point;

   function Get_World_Square
     (P : Point)
      return Chaos.Areas.Area_Square_Location;

   function Get_Square_Centre
     (Location : Chaos.Areas.Area_Square_Location)
      return Point;

   function Get_Square_Centre
     (Location : Chaos.Areas.Area_Square_Location)
      return Chaos.UI.Screens.Point_Type;

   procedure Set_Feature_Background
     (Cr      : Cairo.Cairo_Context;
      Feature : Chaos.Db.Feature_Reference);

   procedure Set_Square_Background
     (Cr               : Cairo.Cairo_Context;
      World_X, World_Y : Chaos.World.World_Coordinate);

   procedure Square_Path
     (Cr : Cairo.Cairo_Context;
      P  : Chaos.Areas.Area_Square_Location);

   procedure Redraw;

   procedure Draw_Model;

   procedure Draw_Creatures;

   procedure Draw_Feedback;

   procedure Draw_Interface;

   procedure Draw_Animation
     (Animation : Chaos.Resources.Animations.Animation_Type);

--     procedure Load_Creatures;

   function Configure_Draw_Area
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Configure)
      return Boolean;

   function Motion_Draw_Area
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Motion)
      return Boolean;

   function Button_Up_Draw_Area
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button)
      return Boolean;

   function Paint_Draw_Area
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cr   : Cairo.Cairo_Context)
      return Boolean;

   function Scroll_Draw_Area
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Scroll)
      return Boolean;

   procedure Creature_Turn
     (Creature : Chaos.Db.Creature_Reference);
   pragma Unreferenced (Creature_Turn);

   procedure Show_Tool_Buttons;

   procedure Command_Feedback_Colour
     (Cr : Cairo.Cairo_Context;
      Command : Chaos.Players.Command_Type;
      Target  : Chaos.Areas.Area_Square_Location);

   procedure On_Attack_Animation
     (Attacker  : Chaos.Db.Creature_Reference;
      Defender  : Chaos.Db.Creature_Reference;
      Power     : Chaos.Db.Power_Reference;
      Path      : Chaos.Areas.Area_Square_Path;
      Animation : Chaos.Resources.Animations.Animation_Type);

   procedure Activate_Animation
     (Path : Chaos.Areas.Area_Square_Path;
      Animation : Chaos.Resources.Animations.Animation_Type);

   ------------------------
   -- Activate_Animation --
   ------------------------

   procedure Activate_Animation
     (Path      : Chaos.Areas.Area_Square_Path;
      Animation : Chaos.Resources.Animations.Animation_Type)
   is
      use Chaos.Resources.Animations;
      use Chaos.UI.Screens;
      Screen_Path : Path_Type;
      D           : constant Coordinate :=
                      Coordinate (Speed (Animation))
                      * Coordinate (Local_Main_Model.Zoom)
                      / 100.0;
   begin
      for I in Path'First .. Path'Last - 1 loop
         declare
            P1     : Point_Type := Get_Square_Centre (Path (I));
            P2     : constant Point_Type := Get_Square_Centre (Path (I + 1));
            DX, DY : Coordinate;
         begin
            Get_Step (P1, P2, D, DX, DY);

            while (P1.X - P2.X) ** 2 + (P1.Y - P2.Y) ** 2 > D * D loop
               P1.X := P1.X + DX;
               P1.Y := P1.Y + DY;
               Append (Screen_Path, P1);
            end loop;
         end;
      end loop;

      Local_Main_Model.Current_Animation := Animation;
      Chaos.Resources.Animations.Start
        (Local_Main_Model.Current_Animation, Screen_Path);

   end Activate_Animation;

   ---------------------
   -- Active_Creature --
   ---------------------

   overriding procedure Active_Creature
     (Model : in out Root_Main_Model;
      Creature : Chaos.Db.Creature_Reference)
   is
      use List_Of_Creatures;
   begin
      declare
         Position   : Cursor := Model.Creature_List.First;
         Initiative : Natural;
      begin

         Model.Creature_List_Mutex.Take;

         if Model.Creature_List.Contains (Creature) then
            Model.Creature_List_Mutex.Release;
            return;
         end if;

         Ada.Text_IO.Put_Line
           ("Active creature: " & Chaos.Creatures.Name (Creature));

         Initiative := Chaos.Creatures.Initiative (Creature);
         if Initiative = 0 then
            Chaos.Creatures.Roll_Initiative (Creature);
            Initiative := Chaos.Creatures.Initiative (Creature);
            Ada.Text_IO.Put_Line
              ("  initiative:" & Natural'Image (Initiative / 100));
         end if;

         while Has_Element (Position) loop
            exit when Chaos.Creatures.Initiative (Element (Position))
              < Initiative;
            Next (Position);
         end loop;

         if Has_Element (Position) then
            Model.Creature_List.Insert (Position, Creature);
         else
            Model.Creature_List.Append (Creature);
         end if;

         if not Model.Active_Creatures.Contains (Creature) then
            Ada.Text_IO.Put_Line
              ("  adding animation data");
            Model.Active_Creatures.Insert
              (Creature,
               (Animation       => Chaos.Creatures.Get_Bam_Resource (Creature),
                Animation_Index => 0));
         end if;

         Model.Creature_List_Mutex.Release;
      end;

      Ada.Text_IO.Put_Line
        ("  redraw creatures");

      Draw_Creatures;

      Model.Draw_Area.Queue_Draw;

      Ada.Text_IO.Put_Line
        ("  finished");
   end Active_Creature;

   ------------------
   -- Acts_Earlier --
   ------------------

   function Acts_Earlier
     (Left, Right : Chaos.Db.Creature_Reference)
      return Boolean
   is
      use type Chaos.Db.Creature_Reference;
   begin
      if Left = Right then
         return False;
      else
         declare
            Creature_1 : constant Chaos.Db.Creature.Creature_Type :=
                           Chaos.Db.Creature.Get (Left);
            Creature_2 : constant Chaos.Db.Creature.Creature_Type :=
                           Chaos.Db.Creature.Get (Left);
         begin
            return Creature_1.Initiative > Creature_2.Initiative;
         end;
      end if;
   end Acts_Earlier;

   -------------------------
   -- Button_Up_Draw_Area --
   -------------------------

   function Button_Up_Draw_Area
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button)
      return Boolean
   is
      pragma Unreferenced (Self);
      use type Chaos.Players.Command_Type;
      Location : constant Chaos.Areas.Area_Square_Location :=
                   Get_World_Square ((Event.X, Event.Y));

   begin
      if Local_Main_Model.Current_Command /= null
        and then Local_Main_Model.Current_Command.Destination_OK (Location)
      then
         Local_Main_Model.Current_Command.Execute (Location);
         Local_Main_Model.Executing_Command :=
           Local_Main_Model.Current_Command;
         if not Local_Main_Model.Current_Command.More then
            Local_Main_Model.Current_Command := null;
         end if;
      end if;
      Update_Command_Buttons;
      Redraw;
      Local_Main_Model.Draw_Area.Queue_Draw;
      return False;
   end Button_Up_Draw_Area;

   -----------------------------
   -- Command_Feedback_Colour --
   -----------------------------

   procedure Command_Feedback_Colour
     (Cr : Cairo.Cairo_Context;
      Command : Chaos.Players.Command_Type;
      Target  : Chaos.Areas.Area_Square_Location)
   is
   begin
      case Command.Get_Path_Feedback (Target) is
         when Chaos.Players.OK =>
            Cairo.Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
         when Chaos.Players.Safe =>
            Cairo.Set_Source_Rgb (Cr, 0.0, 1.0, 0.0);
         when Chaos.Players.Dangerous =>
            Cairo.Set_Source_Rgb (Cr, 1.0, 0.67, 0.0);
         when Chaos.Players.Long =>
            Cairo.Set_Source_Rgb (Cr, 1.0, 1.0, 0.0);
         when Chaos.Players.Attack =>
            Cairo.Set_Source_Rgb (Cr, 1.0, 0.0, 0.0);
      end case;
   end Command_Feedback_Colour;

   -------------------------
   -- Configure_Draw_Area --
   -------------------------

   function Configure_Draw_Area
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Configure)
      return Boolean
   is
      use type Cairo.Cairo_Surface;
      pragma Unreferenced (Event);
      Width, Height : Glib.Gint;
   begin

      if Local_Main_Model.Ground_Surface /= Cairo.Null_Surface then
         Cairo.Surface_Destroy (Local_Main_Model.Ground_Surface);
         Cairo.Surface_Destroy (Local_Main_Model.Creature_Surface);
         Cairo.Surface_Destroy (Local_Main_Model.Feedback_Surface);
         Cairo.Surface_Destroy (Local_Main_Model.Interface_Surface);
         Cairo.Surface_Destroy (Local_Main_Model.Animation_Surface);
      end if;

      Width := Self.Get_Allocated_Width;
      Height := Self.Get_Allocated_Height;

      Local_Main_Model.Width := Glib.Gdouble (Width);
      Local_Main_Model.Height := Glib.Gdouble (Height);

      Local_Main_Model.Ground_Surface :=
        Gdk.Window.Create_Similar_Surface
          (Self.Get_Window,
           Cairo.Cairo_Content_Color,
           Width * 2, Height * 2);

      Local_Main_Model.Creature_Surface :=
        Gdk.Window.Create_Similar_Surface
          (Self.Get_Window,
           Cairo.Cairo_Content_Color_Alpha,
           Width * 2, Height * 2);

      Local_Main_Model.Feedback_Surface :=
        Gdk.Window.Create_Similar_Surface
          (Self.Get_Window,
           Cairo.Cairo_Content_Color_Alpha,
           Width * 2, Height * 2);

      Local_Main_Model.Animation_Surface :=
        Gdk.Window.Create_Similar_Surface
          (Self.Get_Window,
           Cairo.Cairo_Content_Color_Alpha,
           Width * 2, Height * 2);

      Local_Main_Model.Interface_Surface :=
        Gdk.Window.Create_Similar_Surface
          (Self.Get_Window,
           Cairo.Cairo_Content_Color_Alpha,
           Width, Height);

      Redraw;

      return True;

   end Configure_Draw_Area;

   ---------------------------
   -- Create_Command_Button --
   ---------------------------

   function Create_Command_Button
     (Command : Chaos.Players.Command_Type)
      return Gtk.Button.Gtk_Button
   is
      use Glib;
      Button : constant Command_Button_Access := new Command_Button;
   begin
      Button.Initialize
        (Chaos.Localisation.Local_Capital_Text
           (Command.Tag));
      Button.Command := Command;
      Button.On_Clicked (On_Command_Button_Click'Access);

      Button.Set_Size_Request (100, 30);
      Button.Set_Name (Command.Class);
      Button.Get_Child.Set_Name (Command.Class);

      return Gtk.Button.Gtk_Button (Button);
   end Create_Command_Button;

   ------------------
   -- Create_Model --
   ------------------

   procedure Create_Model is
      Top : constant Gtk.Widget.Gtk_Widget :=
              Gtk.Widget.Gtk_Widget
                (UI_Definition.Get_Object ("Main_Model"));
   begin
      Local_Main_Model := new Root_Main_Model;
      Local_Main_Model.Creature_List_Mutex :=
        new Chaos.Mutexes.Mutex;

      Local_Main_Model.Creature_List_Mutex.Take;
      Local_Main_Model.Expand := True;
      Local_Main_Model.Draw_Area :=
        Gtk.Drawing_Area.Gtk_Drawing_Area
          (UI_Definition.Get_Object ("Main_Draw_Area"));
      Local_Main_Model.Feedback_Text_View :=
        Gtk.Text_View.Gtk_Text_View
          (UI_Definition.Get_Object ("Text_Feedback"));
      Local_Main_Model.Tool_Button_Box :=
        Gtk.Box.Gtk_Box
          (UI_Definition.Get_Object ("Tool_Buttons"));

      Local_Main_Model.Draw_Area.On_Configure_Event
        (Configure_Draw_Area'Access);
      Local_Main_Model.Draw_Area.On_Draw
        (Paint_Draw_Area'Access);
      Local_Main_Model.Draw_Area.On_Motion_Notify_Event
        (Motion_Draw_Area'Access);
      Local_Main_Model.Draw_Area.On_Button_Release_Event
        (Button_Up_Draw_Area'Access);
      Local_Main_Model.Draw_Area.On_Scroll_Event
        (Scroll_Draw_Area'Access);

      Local_Main_Model.Feedback_Text_View.Override_Background_Color
        (Gtk.Enums.Gtk_State_Flag_Normal,
         Gdk.RGBA.Black_RGBA);

      Local_Main_Model.Feedback_Text_View.Override_Color
        (Gtk.Enums.Gtk_State_Flag_Normal,
         Gdk.RGBA.White_RGBA);

      Local_Main_Model.Create_Model (Top);

      Local_Main_Model.Creature_List_Mutex.Release;

   end Create_Model;

   -----------------------
   -- Creature_End_Turn --
   -----------------------

   overriding procedure Creature_End_Turn
     (Model    : in out Root_Main_Model;
      Creature : Chaos.Db.Creature_Reference)
   is
      pragma Assert (List_Of_Creatures.Has_Element (Model.Acting_Creature));
      pragma Assert
        (Chaos.Db."="
           (Creature, List_Of_Creatures.Element (Model.Acting_Creature)));
   begin
      Chaos.Creatures.Encounters.End_Turn (Creature);
      List_Of_Creatures.Next (Model.Acting_Creature);
      if List_Of_Creatures.Has_Element (Model.Acting_Creature) then
         Chaos.Creatures.Encounters.Start_Turn
           (List_Of_Creatures.Element (Model.Acting_Creature));
      end if;
      Model.Selected_Player := null;
      Model.Current_Command := null;
   end Creature_End_Turn;

   -------------------
   -- Creature_Turn --
   -------------------

   procedure Creature_Turn
     (Creature : Chaos.Db.Creature_Reference)
   is
      use type Chaos.Players.Player_Type;
      M : Main_Model_Access renames Local_Main_Model;
   begin
      M.Selected_Player := Chaos.Players.Get_Party_Member (Creature);
      if M.Selected_Player /= null then
         Show_Tool_Buttons;
      end if;
   end Creature_Turn;

   -------------------
   -- Creature_Wait --
   -------------------

   overriding procedure Creature_Wait
     (Model    : in out Root_Main_Model;
      Creature : Chaos.Db.Creature_Reference)
   is
      pragma Assert (List_Of_Creatures.Has_Element (Model.Acting_Creature));
      pragma Assert
        (Chaos.Db."="
           (Creature, List_Of_Creatures.Element (Model.Acting_Creature)));
      Position : List_Of_Creatures.Cursor := Model.Acting_Creature;
   begin
      List_Of_Creatures.Next (Model.Acting_Creature);
      Model.Creature_List.Delete (Position);
      Model.Creature_List.Append (Creature);
      if not List_Of_Creatures.Has_Element (Model.Acting_Creature) then
         Model.Acting_Creature := Model.Creature_List.Last;
      else
         Chaos.Creatures.Encounters.Start_Turn
           (List_Of_Creatures.Element (Model.Acting_Creature));
         Model.Selected_Player := null;
         Model.Current_Command := null;
      end if;
   end Creature_Wait;

   -------------------
   -- Creature_Walk --
   -------------------

   overriding procedure Creature_Walk
     (Model    : in out Root_Main_Model;
      Creature : Chaos.Db.Creature_Reference;
      Path     : Chaos.Areas.Area_Square_Path)
   is
   begin
      Model.Walking_Creature := Creature;
      Model.Walking_Path := new Chaos.Areas.Area_Square_Path'(Path);
      Model.Walking_Path_Index := 1;
      Model.Walking_Partial :=
        Get_Square_Centre (Chaos.Creatures.Location (Creature));
   end Creature_Walk;

   --------------------
   -- Draw_Animation --
   --------------------

   procedure Draw_Animation
     (Animation : Chaos.Resources.Animations.Animation_Type)
   is
      P        : Chaos.UI.Screens.Point_Type;
      Resource : Chaos.Resources.Bam.Bam_Resource;
      H, V     : Boolean;
      Cycle    : Natural;
      Index    : Natural;
      Width    : Natural;
      Height   : Natural;
      Surface  : Cairo.Cairo_Surface;

      Cr : constant Cairo.Cairo_Context :=
             Cairo.Create (Local_Main_Model.Animation_Surface);

   begin
      Cairo.Set_Source_Rgba (Cr, 0.0, 0.0, 0.0, 0.0);
      Cairo.Set_Operator (Cr, Cairo.Cairo_Operator_Source);
      Cairo.Paint (Cr);

      if Chaos.Resources.Animations.Active (Animation) then
         Cairo.Set_Operator (Cr, Cairo.Cairo_Operator_Over);

         Chaos.Resources.Animations.Get_Current
           (Animation       => Animation,
            Location        => P,
            Resource        => Resource,
            Flip_Horizontal => H,
            Flip_Vertical   => V,
            Cycle_Index     => Cycle,
            Frame_Index     => Index);

         Chaos.Resources.Bam.Cairo_Bam.Get_Frame_Surface
           (Resource, Cycle, Index, H, V,
            Chaos.Resources.Bam.No_Colour_Replacement,
            Width, Height, Surface);

         if Width > 0 and then Height > 0 then
            Cairo.Set_Source_Surface
              (Cr, Surface,
               Gdouble (P.X) - Gdouble (Width / 2),
               Gdouble (P.Y) - Gdouble (Height) + 7.0);
            Cairo.Paint (Cr);
            Cairo.Surface_Destroy (Surface);
            Local_Main_Model.Animation_Changed := True;
         end if;
      end if;

      Cairo.Destroy (Cr);

   end Draw_Animation;

   --------------------
   -- Draw_Creatures --
   --------------------

   procedure Draw_Creatures is
      use Glib;
      M  : constant Main_Model_Access := Local_Main_Model;
      Cr : constant Cairo.Cairo_Context :=
             Cairo.Create (M.Creature_Surface);
   begin
      Cairo.Set_Source_Rgba (Cr, 0.0, 0.0, 0.0, 0.0);
      Cairo.Set_Operator (Cr, Cairo.Cairo_Operator_Source);
      Cairo.Paint (Cr);
      Cairo.Set_Operator (Cr, Cairo.Cairo_Operator_Over);

      M.Creature_List_Mutex.Take;

      for Ref of M.Creature_List loop
         declare
            Creature : constant Chaos.Db.Creature.Creature_Type :=
                         Chaos.Db.Creature.Get (Ref);
            Actor    : constant Chaos.Db.Actor.Actor_Type :=
                         Chaos.Db.Actor.Get (Creature.Actor);
            Tag      : constant String := Actor.Tag;
            pragma Unreferenced (Tag);
         begin
            if On_Screen (Creature.X, Creature.Y) then
               declare
                  use type Chaos.Resources.Bam.Bam_Resource;
                  use type Cairo.Cairo_Surface;
                  use type Chaos.Db.Creature_Reference;
                  use Glib;
                  P  : constant Point :=
                         (if M.Walking_Creature = Ref
                          then M.Walking_Partial
                          else Get_Square_Centre
                            (Chaos.Creatures.Location (Creature)));
                  Acting : constant Boolean :=
                             List_Of_Creatures.Has_Element
                               (M.Acting_Creature)
                                 and then List_Of_Creatures.Element
                                   (M.Acting_Creature) = Ref;
                  Bam : constant Chaos.Resources.Bam.Bam_Resource :=
                          Chaos.Creatures.Get_Bam_Resource (Ref);
                  Mapped_Anim : constant Integer :=
                                  Direction_To_Animation
                                    (Creature.Facing);
                  Waiting_Offset : constant Integer :=
                                     (if M.Walking_Creature = Ref
                                      then Walking_Offset
                                      else Standing_Offset);
                  Anim_Index  : constant Natural :=
                                  abs Mapped_Anim + Waiting_Offset;
                  Anim_Reverse : constant Boolean := Mapped_Anim < 0;
                  Anim_Frame   : constant Natural :=
                                   (if Acting
                                    then M.Animation_Frame_Index
                                    / Animation_Update_Cycles
                                    else 0);
                  Surface : Cairo.Cairo_Surface;
                  Width, Height : Natural;
               begin
                  if Bam /= null then

                     Chaos.Resources.Bam.Cairo_Bam.Get_Frame_Surface
                       (Bam, Anim_Index, Anim_Frame, Anim_Reverse, False,
                        Chaos.Creatures.Graphics.Get_Creature_Colours
                          (Creature.Actor),
                        Width, Height, Surface);

                     if Width > 0 and then Height > 0 then
                        Cairo.Save (Cr);
                        Cairo.Set_Source_Surface
                          (Cr, Surface,
                           P.X - Gdouble (Width / 2),
                           P.Y - Gdouble (Height) + 7.0);
--                          if Anim_Reverse then
--                             Cairo.Scale (Cr, -1.0, 1.0);
--                          end if;
                        Cairo.Paint (Cr);
                        Cairo.Surface_Destroy (Surface);
                        Cairo.Restore (Cr);
                     else
                        declare
                           R  : constant Gdouble := Gdouble (M.Zoom / 2) * 0.5;
                           Pi : constant := Ada.Numerics.Pi;
                        begin
                           Cairo.Set_Source_Rgb (Cr, 1.0, 0.0, 1.0);
                           Cairo.Arc
                             (Cr     => Cr,
                              Xc     => P.X,
                              Yc     => P.Y,
                              Radius => R,
                              Angle1 => 0.0,
                              Angle2 => 2.0 * Pi);
                           Cairo.Fill (Cr);
                           Cairo.Move_To (Cr, P.X, P.Y);
                           Cairo.Line_To
                             (Cr,
                              P.X + R * 1.5 * DX (Creature.Facing),
                              P.Y + R * 1.5 * DY (Creature.Facing));
                           Cairo.Stroke (Cr);
                        end;
                     end if;
                  end if;
               end;
            end if;
         end;
      end loop;

      M.Creature_List_Mutex.Release;

      Cairo.Destroy (Cr);
      M.Creatures_Changed := False;
      M.Animation_Frame_Index := M.Animation_Frame_Index + 1;

   end Draw_Creatures;

   -------------------
   -- Draw_Feedback --
   -------------------

   procedure Draw_Feedback is
      use Glib;
      use Chaos.Db;
      use Chaos.World;
      use Chaos.Players;
      M  : constant Main_Model_Access := Local_Main_Model;
      Cr : constant Cairo.Cairo_Context :=
             Cairo.Create (M.Feedback_Surface);
   begin
      Cairo.Set_Source_Rgba (Cr, 0.0, 0.0, 0.0, 0.0);
      Cairo.Set_Operator (Cr, Cairo.Cairo_Operator_Source);
      Cairo.Paint (Cr);
      Cairo.Set_Operator (Cr, Cairo.Cairo_Operator_Over);

      declare
         Quad : constant Quadrangle :=
                  Get_Square_Boundary
                    (M.Selected_Square.X, M.Selected_Square.Y);
      begin
         Cairo.Set_Source_Rgb (Cr, 1.0, 0.51, 0.0);
         Cairo.Move_To (Cr, Quad (Quad'Last).X, Quad (Quad'Last).Y);
         for P of Quad loop
            Cairo.Line_To (Cr, P.X, P.Y);
         end loop;
         Cairo.Stroke (Cr);
      end;

      if M.Selected_Player /= null
        and then List_Of_Creatures.Has_Element (M.Acting_Creature)
        and then M.Walking_Creature = Null_Creature_Reference
      then
         declare
            Acting : constant World_Location :=
                       Chaos.Creatures.Location
                         (List_Of_Creatures.Element (M.Acting_Creature));
            Quad   : constant Quadrangle :=
                       Get_Square_Boundary
                         (Acting.X, Acting.Y);
         begin
            Cairo.Set_Source_Rgba (Cr, 0.9, 0.9, 0.9, 0.5);
            Cairo.Move_To (Cr, Quad (Quad'Last).X, Quad (Quad'Last).Y);
            for P of Quad loop
               Cairo.Line_To (Cr, P.X, P.Y);
            end loop;
            Cairo.Fill (Cr);
         end;

      end if;

      if M.Selected_Square /= M.Hover_Square then
         declare
            Quad : constant Quadrangle :=
                     Get_Square_Boundary
                       (M.Hover_Square.X, M.Hover_Square.Y);
         begin
            Cairo.Set_Source_Rgb (Cr, 1.0, 1.0, 0.8);
            Cairo.Move_To (Cr, Quad (Quad'Last).X, Quad (Quad'Last).Y);
            for P of Quad loop
               Cairo.Line_To (Cr, P.X, P.Y);
            end loop;
            Cairo.Stroke (Cr);
         end;

         if M.Current_Command /= null then
            if M.Current_Command.Destination_OK (M.Hover_Square) then
               declare
                  Path : constant World_Path :=
                           M.Current_Command.Path_To (M.Hover_Square);
                  Area : constant World_Path :=
                           M.Current_Command.Area_Effect (M.Hover_Square);
               begin
                  Cairo.Set_Line_Width (Cr, 2.0);
                  Cairo.Set_Line_Cap (Cr, Cairo.Cairo_Line_Cap_Round);
                  Command_Feedback_Colour
                    (Cr, M.Current_Command, M.Hover_Square);

                  for I in Path'Range loop
                     declare
                        P : constant Point :=
                              Get_Square_Centre (Path (I));
                     begin
                        if I = Path'First then
                           Cairo.Move_To (Cr, P.X, P.Y);
                        else
                           Cairo.Line_To (Cr, P.X, P.Y);
                        end if;
                     end;
                  end loop;

                  Cairo.Stroke (Cr);

                  if Area'Length > 0 then
                     Cairo.Set_Source_Rgba
                       (Cr, 1.0, 1.0, 0.0, 0.3);

                     for P of Area loop
                        Square_Path (Cr, P);
                        Cairo.Fill (Cr);
                     end loop;
                  end if;

                  if Path'Length > 1 then
                     declare
                        Creature   : constant Chaos.Db.Creature_Reference :=
                                       List_Of_Creatures.Element
                                         (M.Acting_Creature);
                        Old_Facing : constant Direction :=
                                       Chaos.Creatures.Current_Facing
                                         (Creature);
                     begin
                        Chaos.Creatures.Look_At
                          (Creature, Path (Path'First + 1));

                        if Chaos.Creatures.Current_Facing (Creature)
                          /= Old_Facing
                        then
                           M.Creatures_Changed := True;
                        end if;
                     end;
                  end if;
               end;
            end if;
         end if;
      end if;

      Cairo.Destroy (Cr);
   end Draw_Feedback;

   --------------------
   -- Draw_Interface --
   --------------------

   procedure Draw_Interface is
      use Glib;
      M  : constant Main_Model_Access := Local_Main_Model;
      Cr : constant Cairo.Cairo_Context :=
             Cairo.Create (M.Interface_Surface);
   begin
      Cairo.Set_Source_Rgba (Cr, 0.0, 0.0, 0.0, 0.0);
      Cairo.Set_Operator (Cr, Cairo.Cairo_Operator_Source);
      Cairo.Paint (Cr);
      Cairo.Set_Operator (Cr, Cairo.Cairo_Operator_Over);

      for Party of Chaos.Db.Party.Select_By_Position loop
         declare
            X : constant Gdouble :=
                  Gdouble (M.Draw_Area.Get_Allocated_Width) - 64.0;
            Y : constant Gdouble :=
                  Gdouble (Party.Position - 1) * 64.0;
            Creature : constant Chaos.Db.Creature.Creature_Type :=
                         Chaos.Db.Creature.Get (Party.Creature);
            Image : constant Cairo.Cairo_Surface :=
                      Chaos.UI.Gtk_UI.Resources.Image_Surface
                           (Creature.Tag);
         begin
            Cairo.Set_Source_Surface (Cr, Image, X, Y);
            Cairo.Paint (Cr);
            if Creature.Hp < Creature.Max_Hp then
               if Creature.Hp <= Creature.Max_Hp / 2 then
                  Cairo.Set_Source_Rgba (Cr, 0.5, 0.1, 0.1, 0.5);
               else
                  Cairo.Set_Source_Rgba (Cr, 0.5, 0.5, 0.1, 0.5);
               end if;
               declare
                  Remaining : constant Gdouble :=
                                64.0 * Gdouble (Creature.Hp)
                                / Gdouble (Creature.Max_Hp);
                  Start_Y : constant Gdouble := Y + Remaining;
               begin
                  Cairo.Rectangle (Cr, X, Start_Y, 64.0, 64.0 - Remaining);
                  Cairo.Fill (Cr);
               end;
            end if;
         end;
      end loop;

      Cairo.Destroy (Cr);
   end Draw_Interface;

   ----------------
   -- Draw_Model --
   ----------------

   procedure Draw_Model is
      use Glib;
      M : constant Main_Model_Access := Local_Main_Model;
      Cr       : constant Cairo.Cairo_Context :=
                   Cairo.Create (M.Ground_Surface);

      procedure Draw_Squares;

      ------------------
      -- Draw_Squares --
      ------------------

      procedure Draw_Squares is
         use Chaos.World;
         M : Main_Model_Access renames Local_Main_Model;
         Border : constant Boolean := Natural (M.Height) / M.Zoom < 30;
      begin
         Cairo.Set_Source_Rgba (Cr, 0.5, 0.5, 0.5, 0.8);

         for Y in 1 .. 2 * Natural (M.Height) / M.Zoom loop
            for X in 1 .. 2 * Natural (M.Width) / M.Zoom loop
               declare
                  WX  : constant World_Coordinate :=
                          World_Coordinate (X)
                          - M.Squares_Across / 2
                          + M.Left_X - 1;
                  WY  : constant World_Coordinate :=
                          World_Coordinate (Y)
                          - M.Squares_Down / 2
                          + M.Top_Y - 1;
--                    Alt : constant Chaos.Sectors.Altitude_Range :=
--                            Chaos.World.Altitude (WX, WY);
                  Quad : constant Quadrangle := Get_Square_Boundary (WX, WY);
               begin
                  Cairo.Move_To (Cr, Quad (Quad'Last).X, Quad (Quad'Last).Y);
                  for P of Quad loop
                     Cairo.Line_To (Cr, P.X, P.Y);
                  end loop;

                  Set_Square_Background (Cr, WX, WY);

                  if Border then
                     Cairo.Fill_Preserve (Cr);
                     Cairo.Set_Source_Rgba (Cr, 0.5, 0.5, 0.5, 0.8);
                     Cairo.Stroke (Cr);
                  else
                     Cairo.Fill (Cr);
                  end if;

--                    Cairo.Move_To (Cr, X1, Y1 + Gdouble (M.Zoom));
--                    Cairo.Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
--                    Cairo.Show_Text
--                      (Cr, Integer'Image (Integer (Alt)));

               end;
            end loop;
         end loop;
      end Draw_Squares;

   begin

      Cairo.Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
      Cairo.Paint (Cr);

--        Cairo.Translate (Cr, Gdouble (Width) / 2.0, -Gdouble (Height));
--        Cairo.Rotate (Cr, 68.0 * Ada.Numerics.Pi / 180.0);
      Draw_Squares;

      Cairo.Destroy (Cr);
   end Draw_Model;

   -------------------------
   -- Get_Square_Boundary --
   -------------------------

   function Get_Square_Boundary
     (World_X, World_Y : Chaos.World.World_Coordinate)
      return Quadrangle
   is
      use type Chaos.World.World_Coordinate;
   begin
      return (1 => Get_Square_Top_Left (World_X, World_Y),
              2 => Get_Square_Top_Left (World_X + 1, World_Y),
              3 => Get_Square_Top_Left (World_X + 1, World_Y + 1),
              4 => Get_Square_Top_Left (World_X, World_Y + 1));
   end Get_Square_Boundary;

   -----------------------
   -- Get_Square_Centre --
   -----------------------

   function Get_Square_Centre
     (Location : Chaos.Areas.Area_Square_Location)
      return Point
   is
      use Glib;
      Quad : constant Quadrangle :=
               Get_Square_Boundary (Location.X, Location.Y);
      X, Y : Gdouble := 0.0;
   begin
      for P of Quad loop
         X := X + P.X;
         Y := Y + P.Y;
      end loop;
      return (X / Gdouble (Quad'Length),
              Y / Gdouble (Quad'Length));
   end Get_Square_Centre;

   -----------------------
   -- Get_Square_Centre --
   -----------------------

   function Get_Square_Centre
     (Location : Chaos.Areas.Area_Square_Location)
      return Chaos.UI.Screens.Point_Type
   is
      P : constant Point := Get_Square_Centre (Location);
   begin
      return (Chaos.UI.Screens.Coordinate (P.X),
              Chaos.UI.Screens.Coordinate (P.Y));
   end Get_Square_Centre;

   -------------------------
   -- Get_Square_Top_Left --
   -------------------------

   function Get_Square_Top_Left
     (World_X, World_Y : Chaos.World.World_Coordinate)
      return Point
   is
      use Chaos.World;
      use Glib;
      M : Main_Model_Access renames Local_Main_Model;
      Rel_X : constant World_Coordinate :=
                World_X - M.Left_X + M.Squares_Across / 2;
      Rel_Y : constant World_Coordinate :=
                World_Y - M.Top_Y + M.Squares_Down / 2;
      --  Height : constant World_Coordinate := M.Bottom_Y - M.Top_Y + 1;
      Alt   : constant Chaos.World.Altitude_Range :=
                 Chaos.World.Map.Altitude (World_X, World_Y);
      pragma Unreferenced (Alt);

      X     : constant Gdouble :=
                Gdouble (M.Zoom) * Gdouble (Rel_X);
      Y      : constant Gdouble :=
                 Gdouble (M.Zoom) * Gdouble (Rel_Y);
               --  - Gdouble (M.Zoom) * Gdouble (Alt) / 800.0;
   begin
      return (X, Y);
   end Get_Square_Top_Left;

   ----------------------
   -- Get_World_Square --
   ----------------------

   function Get_World_Square
     (P : Point)
      return Chaos.Areas.Area_Square_Location
   is
      M  : Main_Model_Access renames Local_Main_Model;
      Cr : constant Cairo.Cairo_Context :=
             Cairo.Create (M.Ground_Surface);
      Perspective : aliased Cairo.Cairo_Matrix;
      TX : constant Gdouble :=
                      Gdouble (M.Squares_Across) * Gdouble (M.Zoom)
                    * Translate_X;
      pragma Unreferenced (TX);
      TY          : constant Gdouble :=
                      Gdouble (M.Squares_Down) * Gdouble (M.Zoom)
                    * Translate_Y;
      pragma Unreferenced (TY);
      Map_X       : aliased Glib.Gdouble :=
                      P.X;
      Map_Y       : aliased Glib.Gdouble :=
                      P.Y;
   begin
      Cairo.Matrix.Init
        (Perspective'Access,
         Scale_X, Rotate_Y,
         Rotate_X, Scale_Y,
         0.0, 0.0);
      Cairo.Set_Matrix
        (Cr, Perspective'Access);
      Cairo.Device_To_User
        (Cr, Map_X'Access, Map_Y'Access);
      Cairo.Destroy (Cr);

      declare
         use Chaos.World;
         Result : constant World_Location :=
                    Chaos.World.Map.Ground
                      (M.Left_X
                       + World_Coordinate (Map_X / Glib.Gdouble (M.Zoom)),
                       M.Top_Y
                       + World_Coordinate (Map_Y / Glib.Gdouble (M.Zoom)));
         Centre : constant Point :=
                    Get_Square_Centre (Result);
      begin
         if False then
            Ada.Text_IO.Put_Line
              ("Error: "
               & Integer'Image (Integer (Centre.X - P.X))
               & ", "
               & Integer'Image (Integer (Centre.Y - P.Y))
               & "; size ="
               & Integer'Image (Integer (M.Width))
               & Integer'Image (Integer (M.Height))
               & "; mouse ="
               & Integer'Image (Integer (P.X))
               & Integer'Image (Integer (P.Y))
               & "; altitude ="
               & Altitude_Range'Image (Result.Altitude));
         end if;
         return Result;
      end;
   end Get_World_Square;

   --------------------
   -- Load_Creatures --
   --------------------

--     procedure Load_Creatures is
--     begin
--        Local_Main_Model.Creature_List.Clear;
--        for Creature of Chaos.Db.Creature.Select_By_Top_Record loop
--           if Creature.Hp > 0 then
--              Local_Main_Model.Creature_List.Append (Creature.Reference);
--           end if;
--        end loop;
--        Initiative_Sorting.Sort (Local_Main_Model.Creature_List);
--     end Load_Creatures;

   ---------
   -- Log --
   ---------

   overriding procedure Log
     (Logger : Text_Buffer_Logger;
      Actor  : String;
      Message : String)
   is
      Iter : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Logger.Buffer.Get_End_Iter (Iter);
      Logger.Buffer.Insert
        (Iter, Actor & ": " & Message & Character'Val (10));
      declare
         Mark : constant Gtk.Text_Mark.Gtk_Text_Mark :=
                  Logger.Buffer.Get_Mark ("insert");
      begin
         Logger.View.Scroll_Mark_Onscreen (Mark);
      end;

--        Logger.Buffer.Get_End_Iter (Iter);
--        declare
--           Success : constant Boolean :=
--                       Logger.View.Scroll_To_Iter
--                         (Iter          => Iter,
--                          Within_Margin => 0.0,
--                          Use_Align     => False,
--                          Xalign        => 0.0,
--                          Yalign        => 0.0);
--        begin
--           pragma Unreferenced (Success);
--        end;

   end Log;

   -----------
   -- Model --
   -----------

   function Model return Gtk_UI_Model is
      Sector_X, Sector_Y : constant Integer := 0;
   begin
      if Local_Main_Model = null then
         Create_Model;
      end if;

      Chaos.Creatures.Set_Attack_Animation_Callback
        (On_Attack_Animation'Access);

      declare
         use Chaos.World;
         Protagonist : constant Chaos.Db.Creature.Creature_Type :=
                         Chaos.Db.Creature.Get_By_Player
                           (True);
      begin
         Local_Main_Model.Centre_X := World_Coordinate (Protagonist.X);
         Local_Main_Model.Centre_Y := World_Coordinate (Protagonist.Y);
         Local_Main_Model.Selected_Square :=
           Chaos.World.Map.Ground
             (Local_Main_Model.Centre_X,
              Local_Main_Model.Centre_Y);
         Local_Main_Model.Hover_Square := Local_Main_Model.Selected_Square;
      end;

      Chaos.UI.Logging.Set_Logger
        (new Text_Buffer_Logger'
           (Chaos.UI.Logging.Root_Logger with
                Buffer => Local_Main_Model.Feedback_Text_View.Get_Buffer,
                View   => Local_Main_Model.Feedback_Text_View));

      Local_Main_Model.Action_Timeout_Id :=
        Glib.Main.Timeout_Add (50, On_Action_Timeout'Access);

      for P in Chaos.Players.Party_Member_Index loop
         declare
            use type Chaos.Db.Creature_Reference;
            Creature : constant Chaos.Db.Creature_Reference :=
                         Chaos.Players.Get_Party_Creature (P);
         begin
            if Creature /= Chaos.Db.Null_Creature_Reference then
               Local_Main_Model.Active_Creature (Creature);
            end if;
         end;
      end loop;

      declare
         List : List_Of_Creatures.List;
      begin
         for Creature of
           Chaos.Db.Creature.Select_By_Sector
             (Sector_X, Sector_Y)
         loop
            List.Append (Creature.Reference);
         end loop;

         for Ref of List loop
            Local_Main_Model.Active_Creature (Ref);
         end loop;
      end;

      return Gtk_UI_Model (Local_Main_Model);
   end Model;

   ----------------------
   -- Motion_Draw_Area --
   ----------------------

   function Motion_Draw_Area
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Motion)
      return Boolean
   is
      pragma Unreferenced (Self);
      use Chaos.World;
   begin
      Local_Main_Model.Hover_Square :=
        Get_World_Square ((Event.X, Event.Y));

      if Event.X < Local_Main_Model.Width * 0.1 then
         Local_Main_Model.Current_DX := -1;
      elsif Event.X > Local_Main_Model.Width * 0.9 then
         Local_Main_Model.Current_DX := 1;
      else
         Local_Main_Model.Current_DX := 0;
      end if;

      if Event.Y < Local_Main_Model.Height * 0.1 then
         Local_Main_Model.Current_DY := -1;
      elsif Event.Y > Local_Main_Model.Height * 0.9 then
         Local_Main_Model.Current_DY := 1;
      else
         Local_Main_Model.Current_DY := 0;
      end if;

      Draw_Feedback;
      if Local_Main_Model.Creatures_Changed then
         Draw_Creatures;
      end if;
      Local_Main_Model.Draw_Area.Queue_Draw;
      return False;
   end Motion_Draw_Area;

   -----------------------
   -- On_Action_Timeout --
   -----------------------

   function On_Action_Timeout return Boolean is
      use Chaos.Db;
      use type Chaos.Players.Player_Type;
      use Glib;
      use Chaos.World;
      M : Main_Model_Access renames Local_Main_Model;
      D : constant Gdouble := Gdouble (M.Zoom) / 10.0;
      Full_Redraw : Boolean := False;
   begin
      Chaos.World.Updates.Tick;
      M.Animation_Changed := False;
      M.Animation_Frame_Index :=
        M.Animation_Frame_Index + 1;

      if M.Current_DX /= 0 or else M.Current_DY /= 0 then
         M.Centre_X := M.Centre_X + M.Current_DX;
         M.Centre_Y := M.Centre_Y + M.Current_DY;
         Full_Redraw := True;
      end if;

      if M.Walking_Creature /= Null_Creature_Reference then
         declare
            use type Chaos.Players.Command_Type;
            Target : constant Point :=
                       Get_Square_Centre
                         (M.Walking_Path (M.Walking_Path_Index));
            P      : Point renames M.Walking_Partial;
         begin
            if (P.X - Target.X) ** 2 + (P.Y - Target.Y) ** 2 <= D then
               Chaos.Creatures.Set_Location
                 (M.Walking_Creature,
                  M.Walking_Path (M.Walking_Path_Index));
               M.Walking_Path_Index := M.Walking_Path_Index + 1;
               if M.Walking_Path_Index <= M.Walking_Path'Last then
                  Chaos.Creatures.Look_At
                    (M.Walking_Creature,
                     M.Walking_Path (M.Walking_Path_Index));
               else
                  M.Walking_Path := null;
                  M.Walking_Creature := Null_Creature_Reference;
                  M.Walking_Path_Index := 0;
                  if M.Executing_Command /= null then
                     M.Executing_Command.On_Finish;
                     M.Executing_Command := null;
                  end if;
               end if;
            else
               if P.X < Target.X then
                  P.X := P.X + Gdouble'Min (Target.X - P.X, D);
               elsif P.X > Target.X then
                  P.X := P.X - Gdouble'Min (P.X - Target.X, D);
               end if;
               if P.Y < Target.Y then
                  P.Y := P.Y + Gdouble'Min (Target.Y - P.Y, D);
               elsif P.Y > Target.Y then
                  P.Y := P.Y - Gdouble'Min (P.Y - Target.Y, D);
               end if;
            end if;
         end;

         if not Full_Redraw then
            Draw_Feedback;
            M.Draw_Area.Queue_Draw;
         end if;

      elsif Chaos.Resources.Animations.Active (M.Current_Animation) then
         declare
            use type Chaos.Players.Command_Type;
            Finished : Boolean;
            Changed  : Boolean;
            Extra_Effects : Chaos.Powers.List_Of_Effects.List;
         begin
            Chaos.Resources.Animations.Step
              (M.Current_Animation, Changed, Finished);
            if Finished then
               Draw_Animation (M.Current_Animation);
               Chaos.Powers.Attack
                 (M.Current_Attack.Attacker,
                  M.Current_Attack.Defender,
                  M.Current_Attack.Power,
                  Extra_Effects);
               pragma Assert (Extra_Effects.Is_Empty);
               if not Full_Redraw then
                  Draw_Interface;
               end if;
            elsif Changed and then not Full_Redraw then
               Draw_Animation (M.Current_Animation);
            end if;
         end;
      elsif M.Selected_Player /= null then
         null;
      elsif not List_Of_Creatures.Has_Element (M.Acting_Creature) then

         M.Creature_List_Mutex.Take;
         for Creature of M.Creature_List loop
            Chaos.Creatures.Encounters.End_Round (Creature);
         end loop;
         for Creature of M.Creature_List loop
            Chaos.Creatures.Encounters.Start_Round (Creature);
         end loop;
         M.Acting_Creature := M.Creature_List.First;
         M.Creature_List_Mutex.Release;

         if not Full_Redraw then
            Draw_Feedback;
            Draw_Interface;
         end if;
      else
         declare
            use type Chaos.Players.Player_Type;
            Creature : constant Chaos.Db.Creature_Reference :=
                         List_Of_Creatures.Element (M.Acting_Creature);
            Player   : constant Chaos.Players.Player_Type :=
                         Chaos.Players.Get_Party_Member
                           (Creature);
         begin
            if Player /= null then
               if M.Selected_Player /= Player then
                  M.Selected_Player := Player;
                  Show_Tool_Buttons;
               end if;
            else
               declare
                  Finished : Boolean;
               begin
                  Chaos.Creatures.Update.Act (Creature, Finished);
                  if Finished then
                     Chaos.Creatures.Encounters.End_Turn (Creature);
                     List_Of_Creatures.Next (M.Acting_Creature);
                     if List_Of_Creatures.Has_Element (M.Acting_Creature) then
                        Chaos.Creatures.Encounters.Start_Turn
                          (List_Of_Creatures.Element (M.Acting_Creature));
                     end if;
                     if not Full_Redraw then
                        Draw_Feedback;
                     end if;
                     M.Animation_Frame_Index := 0;
                  end if;
               end;
            end if;
         end;
      end if;

      if Full_Redraw then
         Redraw;
      else
         Draw_Creatures;
      end if;

      Local_Main_Model.Draw_Area.Queue_Draw;

      return True;

   exception

      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Caught exception " & Ada.Exceptions.Exception_Name (E)
            & " in On_Action_Timeout");
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Message: " & Ada.Exceptions.Exception_Message (E));

         return False;

   end On_Action_Timeout;

   -------------------------
   -- On_Attack_Animation --
   -------------------------

   procedure On_Attack_Animation
     (Attacker  : Chaos.Db.Creature_Reference;
      Defender  : Chaos.Db.Creature_Reference;
      Power     : Chaos.Db.Power_Reference;
      Path      : Chaos.Areas.Area_Square_Path;
      Animation : Chaos.Resources.Animations.Animation_Type)
   is
      M : Main_Model_Access renames Local_Main_Model;
   begin
      M.Current_Attack := (Attacker, Defender, Power);
      Activate_Animation (Path, Animation);
   end On_Attack_Animation;

   -----------------------------
   -- On_Command_Button_Click --
   -----------------------------

   procedure On_Command_Button_Click
     (Command_Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      use type Chaos.Players.Command_Type;
      Button : constant Command_Button_Access :=
                 Command_Button_Access (Command_Button);
      Command : constant Chaos.Players.Command_Type := Button.Command;
   begin
      if not Command.Has_Destination then
         Command.Execute;
      else
         if Command = Local_Main_Model.Current_Command  then
            Local_Main_Model.Current_Command := null;
         else
            Local_Main_Model.Current_Command := Command;
         end if;
      end if;

      Update_Command_Buttons;
      Draw_Feedback;
      Local_Main_Model.Draw_Area.Queue_Draw;

   end On_Command_Button_Click;

   -----------------------
   -- On_Creature_Death --
   -----------------------

   overriding procedure On_Creature_Death
     (Model    : Root_Main_Model;
      Creature : Chaos.Db.Creature_Reference)
   is
      use List_Of_Creatures;
   begin
      Model.Creature_List_Mutex.Take;

      declare
         Position : Cursor :=
                      Local_Main_Model.Creature_List.Find
                        (Creature);
      begin
         pragma Assert (Has_Element (Position));
         Local_Main_Model.Creature_List.Delete (Position);
         Model.Creature_List_Mutex.Release;

         Draw_Creatures;
         Local_Main_Model.Draw_Area.Queue_Draw;
      end;
   end On_Creature_Death;

   ---------------------
   -- Paint_Draw_Area --
   ---------------------

   function Paint_Draw_Area
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cr   : Cairo.Cairo_Context)
      return Boolean
   is
      pragma Unreferenced (Self);
      use Glib;
      M : Main_Model_Access renames Local_Main_Model;
      Perspective : aliased Cairo.Cairo_Matrix;
      TX : constant Gdouble :=
                      Gdouble (M.Squares_Across) * Gdouble (M.Zoom)
                    * Translate_X;
      TY          : constant Gdouble :=
                      Gdouble (M.Squares_Down) * Gdouble (M.Zoom)
                    * Translate_Y;

   begin

      Cairo.Save (Cr);
      Cairo.Matrix.Init
        (Perspective'Access,
         Scale_X, Rotate_Y,
         Rotate_X, Scale_Y,
         -TX, -TY);
      Cairo.Set_Matrix
        (Cr, Perspective'Access);
      Cairo.Set_Source_Surface
        (Cr, Local_Main_Model.Ground_Surface, 0.0, 0.0);
      Cairo.Paint (Cr);
      Cairo.Set_Source_Surface
        (Cr, Local_Main_Model.Feedback_Surface, 0.0, 0.0);
      Cairo.Paint (Cr);
      Cairo.Set_Source_Surface
        (Cr, Local_Main_Model.Creature_Surface, 0.0, 0.0);
      Cairo.Paint (Cr);
      Cairo.Set_Source_Surface
        (Cr, Local_Main_Model.Animation_Surface, 0.0, 0.0);
      Cairo.Paint (Cr);
      Cairo.Restore (Cr);

      Cairo.Set_Source_Surface
        (Cr, Local_Main_Model.Interface_Surface, 0.0, 0.0);
      Cairo.Paint (Cr);

      return False;
   end Paint_Draw_Area;

   ------------
   -- Redraw --
   ------------

   procedure Redraw is
      M : Main_Model_Access renames Local_Main_Model;
   begin

      declare
         use Chaos.World;
         Squares_Across : constant World_Coordinate :=
                            World_Coordinate
                              (Natural (M.Width) / M.Zoom + 1);
         Squares_Down : constant World_Coordinate :=
                            World_Coordinate
                              (Natural (M.Height) / M.Zoom + 1);
      begin
         Local_Main_Model.Left_X :=
           Local_Main_Model.Centre_X - Squares_Across / 2 + 1;
         Local_Main_Model.Right_X :=
           Local_Main_Model.Centre_X + Squares_Across / 2 + 1;
         Local_Main_Model.Top_Y :=
           Local_Main_Model.Centre_Y - Squares_Down / 2 + 1;
         Local_Main_Model.Bottom_Y :=
           Local_Main_Model.Centre_Y + Squares_Down / 2 + 1;
         Local_Main_Model.Squares_Across := Squares_Across;
         Local_Main_Model.Squares_Down := Squares_Down;
      end;

      Draw_Model;
      Draw_Creatures;
      Draw_Feedback;
      Draw_Interface;

   end Redraw;

   ---------------------
   -- Remove_Creature --
   ---------------------

   procedure Remove_Creature
     (Model : in out Root_Main_Model'Class;
      Creature : Chaos.Db.Creature.Creature_Type)
   is
   begin
      Model.Creature_List_Mutex.Take;
      declare
         Position : List_Of_Creatures.Cursor :=
                      Model.Creature_List.Find (Creature.Reference);
         pragma Assert (List_Of_Creatures.Has_Element (Position));
      begin
         Model.Creature_List.Delete (Position);
      end;
      Model.Creature_List_Mutex.Release;
   end Remove_Creature;

   ----------------------
   -- Scroll_Draw_Area --
   ----------------------

   function Scroll_Draw_Area
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Scroll)
      return Boolean
   is
      M      : Main_Model_Access renames Local_Main_Model;
      Update : Boolean := False;
   begin
      case Event.Direction is
         when Gdk.Event.Scroll_Down =>
            if M.Zoom > 2 then
               M.Zoom := M.Zoom / 2;
               Update := True;
            end if;
         when Gdk.Event.Scroll_Up =>
            if M.Zoom < 256 then
               M.Zoom := M.Zoom * 2;
               Update := True;
            end if;
         when others =>
            null;
      end case;

      if Update then
         Redraw;
         Self.Queue_Draw;
      end if;

      return True;

   end Scroll_Draw_Area;

   ----------------------------
   -- Set_Feature_Background --
   ----------------------------

   procedure Set_Feature_Background
     (Cr      : Cairo.Cairo_Context;
      Feature : Chaos.Db.Feature_Reference)
   is
      R, G, B, A : Long_Float;
   begin
      Chaos.Features.Get_Colour (Feature, R, G, B, A);
      Cairo.Set_Source_Rgba
        (Cr, Glib.Gdouble (R), Glib.Gdouble (G),
         Glib.Gdouble (B), Glib.Gdouble (A));
   end Set_Feature_Background;

   ---------------------------
   -- Set_Square_Background --
   ---------------------------

   procedure Set_Square_Background
     (Cr               : Cairo.Cairo_Context;
      World_X, World_Y : Chaos.World.World_Coordinate)
   is
      use Glib;
      use Chaos.Db;
      Alt : constant Gdouble :=
              Gdouble
                (Chaos.World.Map.Altitude (World_X, World_Y));
      Feature : constant Chaos.Db.Feature_Reference :=
                  Chaos.World.Map.Feature
                    (Chaos.World.Map.Ground (World_X, World_Y));
   begin
      if Feature /= Null_Feature_Reference then
         Set_Feature_Background (Cr, Feature);
      elsif Alt < 0.0 then
         Cairo.Set_Source_Rgb (Cr, 0.0, 0.0, 1.0 + Alt / 40.0);
      else
         Cairo.Set_Source_Rgb
           (Cr, Alt / 400.0, Alt / 800.0 + 0.5, Alt / 400.0);
      end if;

   end Set_Square_Background;

   -----------------------
   -- Show_Tool_Buttons --
   -----------------------

   procedure Show_Tool_Buttons is
      use type Gtk.Widget.Gtk_Widget;
      M : Main_Model_Access renames Local_Main_Model;
      Commands : constant Chaos.Players.Player_Commands :=
                   M.Selected_Player.Commands;
   begin
      while M.Tool_Button_Box.Get_Child (0) /= null loop
         M.Tool_Button_Box.Remove (M.Tool_Button_Box.Get_Child (0));
      end loop;
      for Item of Commands loop
         M.Tool_Button_Box.Add (Create_Command_Button (Item));
      end loop;
      M.Tool_Button_Box.Show_All;

   end Show_Tool_Buttons;

   -----------------
   -- Square_Path --
   -----------------

   procedure Square_Path
     (Cr : Cairo.Cairo_Context;
      P  : Chaos.Areas.Area_Square_Location)
   is
      Q : constant Quadrangle := Get_Square_Boundary (P.X, P.Y);
   begin
      for I in Q'Range loop
         if I = Q'First then
            Cairo.Move_To (Cr, Q (I).X, Q (I).Y);
         else
            Cairo.Line_To (Cr, Q (I).X, Q (I).Y);
         end if;
      end loop;
   end Square_Path;

   ------------------
   -- Start_Dialog --
   ------------------

   overriding procedure Start_Dialog
     (Model    : in out Root_Main_Model;
      Actor    : Chaos.Db.Creature_Reference;
      Target   : Chaos.Db.Creature_Reference)
   is
      pragma Unreferenced (Actor);
      use type Chaos.Resources.Dialog.Dialog_Resource;
   begin
      Model.Current_Dialog :=
        Chaos.Creatures.Dialog (Target);
      if Model.Current_Dialog /= null then
         Model.Current_Dialog_State := 1;
         Chaos.UI.Logging.Current_Logger.Log
           (Chaos.Creatures.Name (Target),
            Model.Current_Dialog.Actor_Response_Text
              (Model.Current_Dialog_State));
      end if;
   end Start_Dialog;

   ----------------------------
   -- Update_Command_Buttons --
   ----------------------------

   procedure Update_Command_Buttons is
      use Glib;
      M : Main_Model_Access renames Local_Main_Model;
      Index : Gint := 0;
   begin
      loop
         declare
            Child : constant Command_Button_Access :=
                      Command_Button_Access
                        (M.Tool_Button_Box.Get_Child (Index));
         begin
            exit when Child = null;
            Child.Set_Sensitive (Child.Command.Available);
         end;
         Index := Index + 1;
      end loop;
   end Update_Command_Buttons;

end Chaos.UI.Gtk_UI.Main_Model;
