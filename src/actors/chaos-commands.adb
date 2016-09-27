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

end Chaos.Commands;
