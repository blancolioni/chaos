with Chaos.Tokens;

package body Chaos.Actors.Encounters is

   -------------------
   -- End_Encounter --
   -------------------

   procedure End_Encounter
     (Creature : Chaos.Actors.Chaos_Actor)
   is
   begin
      Chaos.Tokens.On_Encounter_End (Creature);
      Chaos.Db.Creature.Get (Creature).Set_Battle (False);
   end End_Encounter;

   ---------------
   -- End_Round --
   ---------------

   procedure End_Round
     (Creature : Chaos.Actors.Chaos_Actor)
   is
   begin
      Chaos.Tokens.On_Round_End (Creature);
   end End_Round;

   --------------
   -- End_Turn --
   --------------

   procedure End_Turn
     (Creature : Chaos.Actors.Chaos_Actor)
   is
   begin
      Chaos.Tokens.On_Turn_End (Creature);
   end End_Turn;

   ---------------------
   -- Start_Encounter --
   ---------------------

   procedure Start_Encounter
     (Creature : Chaos.Actors.Chaos_Actor)
   is
   begin
      Chaos.Tokens.On_Encounter_Start (Creature);
      Chaos.Db.Creature.Get (Creature).Set_Battle (True);
   end Start_Encounter;

   -----------------
   -- Start_Round --
   -----------------

   procedure Start_Round
     (Creature : Chaos.Actors.Chaos_Actor)
   is
   begin
      Chaos.Tokens.On_Round_Start (Creature);
   end Start_Round;

   ----------------
   -- Start_Turn --
   ----------------

   procedure Start_Turn
     (Creature : Chaos.Actors.Chaos_Actor)
   is
      Rec : Chaos.Db.Creature.Creature_Type :=
              Chaos.Db.Creature.Get (Creature);
   begin
      Rec.Set_Move (True);
      Rec.Set_Minor (True);
      Rec.Set_Standard (True);
      Chaos.Tokens.On_Turn_Start (Creature);
   end Start_Turn;

end Chaos.Actors.Encounters;
