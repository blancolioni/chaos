with Chaos.Items.Db;

with Chaos.Entities.Search;

package body Chaos.Items is

   ------------
   -- Create --
   ------------

   function Create
     (Entity : not null access constant
        Chaos.Entities.Chaos_Entity_Record'Class)
      return Chaos_Item
   is
      procedure Create (Item : in out Chaos_Item_Record'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Item : in out Chaos_Item_Record'Class) is
      begin
         Item.Entity := Chaos.Entities.Chaos_Entity (Entity);
      end Create;

   begin
      return Db.Create (Create'Access);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Entity_Code : String)
      return Chaos_Item
   is
      use Chaos.Entities;
      Entity : constant Chaos_Entity :=
                 Chaos.Entities.Search.Get_Entity (Entity_Code);
   begin
      if Entity = null then
         raise Constraint_Error with
           "no such entity: " & Entity_Code;
      end if;
      return Create (Entity);
   end Create;

   ------------
   -- Entity --
   ------------

   function Entity
     (Item : Chaos_Item_Record)
      return Chaos.Entities.Chaos_Entity
   is
   begin
      return Item.Entity;
   end Entity;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Object : Chaos_Item_Record)
      return Memor.Memor_Database
   is
      pragma Unreferenced (Object);
   begin
      return Db.Get_Database;
   end Object_Database;

end Chaos.Items;
