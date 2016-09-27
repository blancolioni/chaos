with Memor.Database;

private package Chaos.Actors.Db is
  new Memor.Database ("actor", Chaos_Actor_Record, Chaos_Actor);
