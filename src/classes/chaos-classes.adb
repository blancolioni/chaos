with Chaos.Classes.Db;

package body Chaos.Classes is

   ---------------
   -- Add_Power --
   ---------------

   overriding procedure Add_Power
     (Class      : in out Chaos_Class_Record;
      Power      : Chaos.Powers.Chaos_Power)
   is
   begin
      Class.Powers.Add_Power (Power);
   end Add_Power;

   ---------------------
   -- Base_Hit_Points --
   ---------------------

   function Base_Hit_Points
     (Class : Chaos_Class_Record'Class)
      return Natural
   is
   begin
      return Class.Base_Hit_Points;
   end Base_Hit_Points;

   ---------------------
   -- Base_Hit_Points --
   ---------------------

   function Base_Hit_Points
     (Item : Chaos_Class_Interface'Class)
      return Natural
   is
   begin
      return Item.Class.Base_Hit_Points;
   end Base_Hit_Points;

   -------------------------
   -- Class_Defence_Bonus --
   -------------------------

   function Class_Defence_Bonus
     (Item    : Chaos_Class_Interface'Class;
      Defence : Chaos.Defences.Defence)
      return Chaos.Defences.Defence_Score_Change
   is
   begin
      if Item.Class /= null then
         return Item.Class.Defences (Defence);
      else
         return 0;
      end if;
   end Class_Defence_Bonus;

   ---------
   -- Get --
   ---------

   function Get (Identifier : String) return Chaos_Class is
   begin
      return Db.Get (Identifier);
   end Get;

   ---------------
   -- Get_Power --
   ---------------

   overriding function Get_Power
     (Class      : Chaos_Class_Record;
      Index      : Positive)
      return Chaos.Powers.Chaos_Power
   is
   begin
      return Class.Powers.Get_Power (Index);
   end Get_Power;

   ----------------------
   -- Level_Hit_Points --
   ----------------------

   function Level_Hit_Points
     (Class : Chaos_Class_Record'Class)
      return Natural
   is
   begin
      return Class.Level_Hit_Points;
   end Level_Hit_Points;

   ----------------------
   -- Level_Hit_Points --
   ----------------------

   function Level_Hit_Points
     (Item : Chaos_Class_Interface'Class)
      return Natural
   is
   begin
      return Item.Class.Level_Hit_Points;
   end Level_Hit_Points;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Object : Chaos_Class_Record)
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
     (Class : Chaos_Class_Record)
      return Natural
   is
   begin
      return Class.Powers.Power_Count;
   end Power_Count;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Process : not null access
        procedure (Class : Chaos_Class))
   is
   begin
      Db.Scan (Process);
   end Scan;

end Chaos.Classes;
