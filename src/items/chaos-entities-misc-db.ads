with Memor.Database;

private package Chaos.Entities.Misc.Db is
  new Memor.Database ("misc",
                      Chaos_Misc_Entity_Record,
                      Chaos_Misc_Entity);
