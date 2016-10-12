with Chaos.Teams.Db;

package body Chaos.Teams is

   --------------
   -- Attitude --
   --------------

   function Attitude (From, To : Chaos_Team) return Chaos_Attitude is
   begin
      for Att in Chaos_Attitude loop
         for T of From.Attitudes (Att) loop
            if T = To then
               return Att;
            end if;
         end loop;
      end loop;

      return Neutral;

   end Attitude;

   ---------
   -- Get --
   ---------

   function Get (Identifier : String) return Chaos_Team is
   begin
      return Db.Get (Identifier);
   end Get;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Object : Chaos_Team_Record)
      return Memor.Memor_Database
   is
      pragma Unreferenced (Object);
   begin
      return Db.Get_Database;
   end Object_Database;

end Chaos.Teams;
