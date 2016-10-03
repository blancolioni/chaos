with Chaos.Things.Db;

package body Chaos.Things is

   ------------
   -- Create --
   ------------

   function Create
     (Item : not null access constant Chaos.Items.Chaos_Item_Record'Class)
      return Chaos_Thing
   is
      procedure Create (Thing : in out Chaos_Thing_Record'Class);

      ------------
      -- Create --
      ------------

      procedure Create (Thing : in out Chaos_Thing_Record'Class) is
      begin
         Thing.Item := Chaos.Items.Chaos_Item (Item);
      end Create;

   begin
      return Db.Create (Create'Access);
   end Create;

   ----------
   -- Item --
   ----------

   function Item
     (Thing : Chaos_Thing_Record)
      return Chaos.Items.Chaos_Item
   is
   begin
      return Thing.Item;
   end Item;

   ---------------------
   -- Object_Database --
   ---------------------

   overriding function Object_Database
     (Object : Chaos_Thing_Record)
      return Memor.Root_Database_Type'Class
   is
      pragma Unreferenced (Object);
   begin
      return Db.Get_Database;
   end Object_Database;

end Chaos.Things;
