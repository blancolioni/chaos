with Chaos.Teams.Db;

package body Chaos.Teams.Configure is

   procedure Create_Team (Identifier : String);

   -----------------
   -- Create_Team --
   -----------------

   procedure Create_Team (Identifier : String) is

      procedure Create (Team : in out Chaos.Teams.Chaos_Team_Record'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Team : in out Chaos.Teams.Chaos_Team_Record'Class) is
      begin
         Team.Initialize (Identifier);
      end Create;

   begin
      Db.Create (Create'Access);
   end Create_Team;

   -----------------
   -- Read_Config --
   -----------------

   procedure Read_Config is
   begin
      Create_Team ("innocent");
      Create_Team ("pc");
      Create_Team ("familiar");
      Create_Team ("allied");
      Create_Team ("controlled");
      Create_Team ("charmed");
      Create_Team ("neutral");
      Create_Team ("enemy");
   end Read_Config;

end Chaos.Teams.Configure;
