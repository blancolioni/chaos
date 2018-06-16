package body Chaos.Commands is

   ------------
   -- Append --
   ------------

   procedure Append
     (Collection : in out Command_Collection;
      Command    : Command_Type)
   is
   begin
      Collection.Vector.Append (Command);
   end Append;

   -----------------
   -- Area_Effect --
   -----------------

   function Area_Effect
     (Command  : Root_Command_Type;
      Location : Chaos.Locations.Square_Location)
      return Chaos.Locations.Square_Path
   is
      pragma Unreferenced (Command, Location);
   begin
      return Chaos.Locations.No_Path;
   end Area_Effect;

   -------------
   -- Command --
   -------------

   function Command
     (Collection : Command_Collection;
      Index      : Positive)
      return Command_Type
   is
   begin
      return Collection.Vector.Element (Index);
   end Command;

   -----------
   -- Count --
   -----------

   function Count (Collection : Command_Collection) return Natural is
   begin
      return Collection.Vector.Last_Index;
   end Count;

   --------------------
   -- Destination_OK --
   --------------------

   function Destination_OK
     (Command  : Root_Command_Type;
      Location : Chaos.Locations.Square_Location)
      return Boolean
   is
      pragma Unreferenced (Command, Location);
   begin
      return False;
   end Destination_OK;

   -----------------------
   -- Get_Path_Feedback --
   -----------------------

   function Get_Path_Feedback
     (Command   : Root_Command_Type;
      Location  : Chaos.Locations.Square_Location)
      return Path_Feedback
   is
      pragma Unreferenced (Command, Location);
   begin
      return OK;
   end Get_Path_Feedback;

end Chaos.Commands;
