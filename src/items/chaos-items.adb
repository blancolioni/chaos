with Chaos.Items.Db;

with Chaos.Entities.Search;

package body Chaos.Items is

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item
     (Inv  : in out Inventory_Interface'Class;
      Item : Chaos_Item)
   is
   begin
      for I in 1 .. Inv.Capacity loop
         if Inv.Item (I) = null then
            Inv.Replace_Item (I, Item);
            return;
         end if;
      end loop;
      raise Inventory_Full with
        "no room for " & Item.Identifier;
   end Add_Item;

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

   ----------------
   -- Has_Entity --
   ----------------

   function Has_Entity
     (Inv    : Inventory_Interface'Class;
      Entity : Chaos.Entities.Chaos_Entity)
      return Boolean
   is
   begin
      for Index in 1 .. Inv.Capacity loop
         declare
            use type Chaos.Entities.Chaos_Entity;
            Item : constant Chaos_Item := Inv.Item (Index);
         begin
            if Item /= null and then Item.Entity = Entity then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Has_Entity;

   --------------
   -- Has_Item --
   --------------

   function Has_Item
     (Inv  : Inventory_Interface'Class;
      Item : Chaos_Item)
      return Boolean
   is
   begin
      for Index in 1 .. Inv.Capacity loop
         if Inv.Item (Index) = Item then
            return True;
         end if;
      end loop;
      return False;
   end Has_Item;

   ----------
   -- Item --
   ----------

   function Item
     (Inv    : Inventory_Interface'Class;
      Entity : Chaos.Entities.Chaos_Entity)
      return Chaos_Item
   is
      use type Chaos.Entities.Chaos_Entity;
   begin
      for Index in 1 .. Inv.Capacity loop
         if Inv.Item (Index) /= null
           and then Inv.Item (Index).Entity = Entity
         then
            return Inv.Item (Index);
         end if;
      end loop;
      raise Constraint_Error with
        "no such item '" & Entity.Identifier & "' in inventory";
   end Item;

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

   -------------------
   -- Remove_Entity --
   -------------------

   procedure Remove_Entity
     (Inv    : in out Inventory_Interface'Class;
      Entity : Chaos.Entities.Chaos_Entity)
   is
      use type Chaos.Entities.Chaos_Entity;
   begin
      for Index in 1 .. Inv.Capacity loop
         if Inv.Item (Index) /= null
           and then Inv.Item (Index).Entity = Entity
         then
            Inv.Replace_Item (Index, null);
            return;
         end if;
      end loop;
      raise Constraint_Error with
        "expected to find " & Entity.Identifier;
   end Remove_Entity;

   -----------------
   -- Remove_Item --
   -----------------

   procedure Remove_Item
     (Inv  : in out Inventory_Interface'Class;
      Item : Chaos_Item)
   is
   begin
      for Index in 1 .. Inv.Capacity loop
         if Inv.Item (Index) = Item then
            Inv.Replace_Item (Index, null);
            return;
         end if;
      end loop;
      raise Constraint_Error with
        "expected to find " & Item.Entity.Identifier;
   end Remove_Item;

   ------------
   -- Weight --
   ------------

   function Weight
     (Inv : Inventory_Interface'Class)
      return Chaos.Weight.Chaos_Weight
   is
      use type Chaos.Weight.Chaos_Weight;
      Result : Chaos.Weight.Chaos_Weight := 0.0;
   begin
      for I in 1 .. Inv.Capacity loop
         if Inv.Item (I) /= null then
            Result := Result + Inv.Item (I).Weight;
         end if;
      end loop;
      return Result;
   end Weight;

end Chaos.Items;
