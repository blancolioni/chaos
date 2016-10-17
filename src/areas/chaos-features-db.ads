with Memor.Database;

private package Chaos.Features.Db is
  new Memor.Database ("feature", Chaos_Feature_Record, Chaos_Feature);
