with Chaos.Powers.Db;

package body Chaos.Powers is

   ---------------
   -- Add_Power --
   ---------------

   overriding procedure Add_Power
     (Collection : in out Power_Collection;
      Power      : Chaos_Power)
   is
   begin
      Collection.Powers.Append (Power);
   end Add_Power;

   ------------
   -- Attack --
   ------------

   procedure Attack
     (Power    : Chaos_Power_Record'Class;
      Attacker : not null access constant
        Chaos.Actors.Chaos_Actor_Record'Class;
      Defender : not null access constant
        Chaos.Actors.Chaos_Actor_Record'Class)
   is null;

   ------------
   -- Attack --
   ------------

   procedure Attack
     (Power    : Chaos_Power_Record'Class;
      Attacker : not null access constant
        Chaos.Actors.Chaos_Actor_Record'Class;
      Target   : Chaos.Locations.Square_Location)
   is null;

   -------------------
   -- Attack_Target --
   -------------------

   function Attack_Target
     (Power : Chaos_Power_Record'Class)
      return Power_Attack_Target
   is
   begin
      return Power.Target;
   end Attack_Target;

   -----------
   -- Class --
   -----------

   function Class
     (Power : Chaos_Power_Record'Class)
      return Power_Class
   is
   begin
      return Power.Class;
   end Class;

   ---------
   -- Get --
   ---------

   function Get (Identifier : String) return Chaos_Power is
   begin
      return Db.Get (Identifier);
   end Get;

   ---------------
   -- Get_Power --
   ---------------

   overriding function Get_Power
     (Collection : Power_Collection;
      Index      : Positive)
      return Chaos_Power
   is
   begin
      return Collection.Powers.Element (Index);
   end Get_Power;

   ----------------
   -- Long_Range --
   ----------------

   function Long_Range (Power : Chaos_Power_Record'Class) return Natural is
   begin
      return Power.Long_Range;
   end Long_Range;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Object : Chaos_Power_Record)
      return Memor.Root_Database_Type'Class
   is
      pragma Unreferenced (Object);
   begin
      return Db.Get_Database;
   end Object_Database;

   -----------------
   -- Power_Count --
   -----------------

   overriding function Power_Count
     (Collection : Power_Collection)
      return Natural
   is
   begin
      return Collection.Powers.Last_Index;
   end Power_Count;

   -----------------
   -- Short_Range --
   -----------------

   function Short_Range (Power : Chaos_Power_Record'Class) return Natural is
   begin
      return Power.Short_Range;
   end Short_Range;

   ---------------
   -- Use_Class --
   ---------------

   function Use_Class
     (Power : Chaos_Power_Record'Class)
      return Power_Use_Class
   is
   begin
      return Power.Use_Class;
   end Use_Class;

end Chaos.Powers;
