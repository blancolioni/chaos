with WL.String_Maps;

package body Chaos.Entities is

   package Entity_Registry_Maps is
     new WL.String_Maps (Chaos_Entity);

   Entity_Registry : Entity_Registry_Maps.Map;

   --------------------
   -- Add_Properties --
   --------------------

   overriding procedure Add_Properties
     (Entity : Chaos_Entity_Record)
   is
      pragma Unreferenced (Entity);
   begin
      null;
   end Add_Properties;

   ------------
   -- Exists --
   ------------

   function Exists
     (Code : String)
      return Boolean
   is
   begin
      return Entity_Registry.Contains (Code);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get
     (Code : String)
      return Chaos_Entity
   is
   begin
      return Entity_Registry.Element (Code);
   end Get;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Entity               : in out Chaos_Entity_Record'Class;
      Code                 : String;
      Needs_Identification : Boolean;
      Identified_Name      : WL.Binary_IO.Word_32;
      Unidentified_Name    : WL.Binary_IO.Word_32;
      Identified_Desc      : WL.Binary_IO.Word_32;
      Unidentified_Desc    : WL.Binary_IO.Word_32)
   is
      use WL.Binary_IO;
      use Chaos.Localisation;
   begin
      Entity.Initialize (Code);
      Entity.Needs_Identification := Needs_Identification;
      Entity.Identified_Name :=  Local_Text_Index (Identified_Name);
      if Identified_Desc /= WL.Binary_IO.Word_32'Last then
         Entity.Identified_Desc := Local_Text_Index (Identified_Desc);
      else
         Entity.Identified_Desc := Entity.Identified_Name;
      end if;
      if Entity.Needs_Identification then
         Entity.Unidentified_Name := Local_Text_Index (Unidentified_Name);
         Entity.Unidentified_Desc := Local_Text_Index (Unidentified_Desc);
      end if;
   end Initialize;

   -----------
   -- Price --
   -----------

   function Price
     (Entity : Chaos_Entity_Record)
      return Chaos.Coins.Coins_Type
   is
   begin
      return Entity.Price;
   end Price;

   ---------------------
   -- Register_Entity --
   ---------------------

   procedure Register_Entity
     (Code   : String;
      Entity : Chaos_Entity)
   is
   begin
      Entity_Registry.Insert (Code, Entity);
   end Register_Entity;

   ------------
   -- Weight --
   ------------

   function Weight
     (Entity : Chaos_Entity_Record)
      return Chaos.Weight.Chaos_Weight
   is
   begin
      return Entity.Weight;
   end Weight;

end Chaos.Entities;
