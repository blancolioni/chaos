with Ada.Characters.Handling;

with Chaos.Races.Db;

package body Chaos.Races is

   -------------------
   -- Ability_Bonus --
   -------------------

   function Ability_Bonus
     (Race    : Chaos_Race_Record;
      Ability : Chaos.Abilities.Ability)
      return Chaos.Abilities.Ability_Score_Change
   is
   begin
      return Race.Abilities (Ability);
   end Ability_Bonus;

   --------------------
   -- Animation_Code --
   --------------------

   overriding function Animation_Code
     (Race : Chaos_Race_Record)
      return Character
   is
      Id : constant String := Race.Identifier;
   begin
      return Ada.Characters.Handling.To_Upper (Id (Id'First));
   end Animation_Code;

   ---------
   -- Get --
   ---------

   function Get (Identifier : String) return Chaos_Race is
   begin
      return Db.Get (Identifier);
   end Get;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Object : Chaos_Race_Record)
      return Memor.Root_Database_Type'Class
   is
      pragma Unreferenced (Object);
   begin
      return Db.Get_Database;
   end Object_Database;

   ----------
   -- Size --
   ----------

   overriding function Size
     (Race : Chaos_Race_Record)
      return Chaos.Sizes.Chaos_Size
   is
   begin
      return Race.Size;
   end Size;

   -----------
   -- Speed --
   -----------

   overriding function Speed
     (Race : Chaos_Race_Record)
      return Chaos.Speed.Chaos_Speed
   is
   begin
      return Race.Speed;
   end Speed;

   ------------
   -- Vision --
   ------------

   overriding function Vision
     (Race : Chaos_Race_Record)
      return Chaos.Vision.Chaos_Vision
   is
   begin
      return Race.Vision;
   end Vision;

end Chaos.Races;
