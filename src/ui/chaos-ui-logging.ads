with Chaos.Powers;

package Chaos.UI.Logging is

   type Root_Logger is abstract tagged private;

   procedure Log
     (Logger  : Root_Logger;
      Actor   : String;
      Message : String)
   is abstract;

   procedure Log_Attack
     (Logger   : Root_Logger;
      Attacker : Chaos.Actors.Chaos_Actor;
      Defender : Chaos.Actors.Chaos_Actor;
      Power    : Chaos.Powers.Chaos_Power);

   procedure Log_Attack_Roll
     (Logger        : Root_Logger;
      Attacker      : Chaos.Actors.Chaos_Actor;
      Attack_Roll   : Positive;
      Attack_Bonus  : Integer;
      Defence       : Integer;
      Hit, Critical : Boolean;
      Fumble        : Boolean);

   procedure Log_Damage
     (Logger       : Root_Logger;
      Attacker     : Chaos.Actors.Chaos_Actor;
      Defender     : Chaos.Actors.Chaos_Actor;
      Damage       : Natural;
      Resisted     : Natural);

   procedure Log_Healing
     (Logger       : Root_Logger;
      Actor        : Chaos.Actors.Chaos_Actor;
      Points       : Natural);

   procedure Log_Death
     (Logger       : Root_Logger;
      Defender     : Chaos.Actors.Chaos_Actor);

   procedure Log_Status
     (Logger    : Root_Logger;
      Defender  : Chaos.Actors.Chaos_Actor;
      Status    : String);

--     procedure Log_Token_Expires
--       (Logger    : Root_Logger;
--        Creature  : Chaos.Actors.Chaos_Actor;
--        Token     : Chaos.Db.Token_Reference);

   type Chaos_Logger is access all Root_Logger'Class;

   procedure Set_Logger (Logger : Chaos_Logger);
   function Current_Logger return Chaos_Logger;

private

   type Root_Logger is abstract tagged null record;

end Chaos.UI.Logging;
