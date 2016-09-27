with Ada.Containers.Vectors;
with Chaos.Db; use Chaos.Db;

package Chaos.Actors.Vectors is
  new Ada.Containers.Vectors (Positive, Chaos.Actors.Chaos_Actor);
