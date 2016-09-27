with Ada.Text_IO;

with Chaos.Creatures;

package body Chaos.UI.Logging is

   type Standard_Output_Logger is
     new Root_Logger with null record;

   overriding procedure Log
     (Logger  : Standard_Output_Logger;
      Actor   : String;
      Message : String);

   Local_Current_Logger : Chaos_Logger := new Standard_Output_Logger;

   --------------------
   -- Current_Logger --
   --------------------

   function Current_Logger return Chaos_Logger is
   begin
      return Local_Current_Logger;
   end Current_Logger;

   ---------
   -- Log --
   ---------

   overriding procedure Log
     (Logger  : Standard_Output_Logger;
      Actor   : String;
      Message : String)
   is
      pragma Unreferenced (Logger);
   begin
      Ada.Text_IO.Put_Line
        (Actor & ": " & Message);
   end Log;

   ----------------
   -- Log_Attack --
   ----------------

   procedure Log_Attack
     (Logger   : Root_Logger;
      Attacker : Chaos.Actors.Chaos_Actor;
      Defender : Chaos.Actors.Chaos_Actor;
      Power    : Chaos.Powers.Chaos_Power)
   is
   begin
      Root_Logger'Class (Logger).Log
        (Attacker.Short_Name, "attacks "
         & Defender.Short_Name
         & " with "
         & Power.Display_Name);
   end Log_Attack;

   ---------------------
   -- Log_Attack_Roll --
   ---------------------

   procedure Log_Attack_Roll
     (Logger        : Root_Logger;
      Attacker      : Chaos.Actors.Chaos_Actor;
      Attack_Roll   : Positive;
      Attack_Bonus  : Integer;
      Defence       : Integer;
      Hit, Critical : Boolean;
      Fumble        : Boolean)
   is
   begin
      Root_Logger'Class (Logger).Log
        (Attacker.Short_Name, "attack roll"
         & Positive'Image (Attack_Roll)
         & (if Attack_Bonus < 0
           then " -" & Positive'Image (-Attack_Bonus)
           else " +" & Natural'Image (Attack_Bonus))
         & " vs defence"
         & Integer'Image (Defence)
         & ": "
         & (if Critical
           then "CRITICAL"
           elsif Hit then "Hit!"
           elsif Fumble then "Fumble!"
           else "Miss"));
   end Log_Attack_Roll;

   ----------------
   -- Log_Damage --
   ----------------

   procedure Log_Damage
     (Logger       : Root_Logger;
      Attacker     : Chaos.Actors.Chaos_Actor;
      Defender     : Chaos.Actors.Chaos_Actor;
      Damage       : Natural;
      Resisted     : Natural)
   is
   begin
      Root_Logger'Class (Logger).Log
        (Attacker.Short_Name, "did"
         & Natural'Image (Damage)
         & (if Resisted > 0
           then "(" & Natural'Image (Resisted) & " resisted)"
           else "")
         & " damage to " & Defender.Short_Name);
   end Log_Damage;

   ---------------
   -- Log_Death --
   ---------------

   procedure Log_Death
     (Logger       : Root_Logger;
      Defender     : Chaos.Actors.Chaos_Actor)
   is
   begin
      Root_Logger'Class (Logger).Log
        (Defender.Short_Name, "death");
   end Log_Death;

   -----------------
   -- Log_Healing --
   -----------------

   procedure Log_Healing
     (Logger       : Root_Logger;
      Actor        : Chaos.Actors.Chaos_Actor;
      Points       : Natural)
   is
   begin
      Root_Logger'Class (Logger).Log
        (Actor.Short_Name, " heals"
         & Natural'Image (Points)
         & "HP");
   end Log_Healing;

   ----------------
   -- Log_Status --
   ----------------

   procedure Log_Status
     (Logger       : Root_Logger;
      Defender  : Chaos.Actors.Chaos_Actor;
      Status    : String)
   is
   begin
      Root_Logger'Class (Logger).Log
        (Defender.Short_Name, Status);
   end Log_Status;

   -----------------------
   -- Log_Token_Expires --
   -----------------------

--     procedure Log_Token_Expires
--       (Logger    : Root_Logger;
--        Creature  : Chaos.Actors.Chaos_Actor;
--        Token     : Chaos.Db.Token_Reference)
--     is
--     begin
--        Root_Logger'Class (Logger).Log
--          (Chaos.Creatures.Name (Creature),
--           Chaos.Localisation.Local_Text
--             (Chaos.Tokens.Expire_Tag (Token)));
--     end Log_Token_Expires;

   ----------------
   -- Set_Logger --
   ----------------

   procedure Set_Logger (Logger : Chaos_Logger) is
   begin
      Local_Current_Logger := Logger;
   end Set_Logger;

end Chaos.UI.Logging;
