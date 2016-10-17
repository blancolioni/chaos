with Memor.Database;

private package Chaos.Items.Db is
  new Memor.Database ("item", Chaos_Item_Record, Chaos_Item);
