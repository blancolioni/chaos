with Chaos.Powers.Db;

with Chaos.Objects.Enum_Properties;

package body Chaos.Powers is

   package Power_Class_Properties is
     new Chaos.Objects.Enum_Properties
       (Chaos_Power_Record, Power_Class, Class);

   package Power_Source_Properties is
     new Chaos.Objects.Enum_Properties
       (Chaos_Power_Record, Power_Source, Source);

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

   --------------------
   -- Add_Properties --
   --------------------

   overriding procedure Add_Properties
     (Object : Chaos_Power_Record)
   is
   begin
      Object.Add_Property ("class",
                           Power_Class_Properties.Get_Property'Access);
      Object.Add_Property ("source",
                           Power_Source_Properties.Get_Property'Access);
   end Add_Properties;

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

   ----------
   -- Mark --
   ----------

   overriding procedure Mark
     (Power      : in out Chaos_Power_Record;
      Mark_Value : not null access
        procedure (Value : in out Lith.Objects.Object))
   is
   begin
      Mark_Value (Power.Attack);
      Mark_Value (Power.Defence);
      Mark_Value (Power.Hit_Effects);
      Mark_Value (Power.Miss_Effects);
      Mark_Value (Power.Effects);
   end Mark;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Object : Chaos_Power_Record)
      return Memor.Memor_Database
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

   ------------
   -- Source --
   ------------

   function Source
     (Power : Chaos_Power_Record'Class)
      return Power_Source
   is
   begin
      return Power.Source;
   end Source;

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
