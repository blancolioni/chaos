with Memor.Database;

private package Chaos.Things.Db is
  new Memor.Database ("thing", Chaos_Thing_Record, Chaos_Thing);
