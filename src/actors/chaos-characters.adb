with Ada.Text_IO;

with Chaos.Dice;

with Chaos.Db.Ability;
with Chaos.Db.Ability_Score;
with Chaos.Db.Actor;
with Chaos.Db.Class;

package body Chaos.Characters is

   function Choose_Class
     (Actor : Chaos.Db.Actor.Actor_Type)
      return Natural;

   ------------------
   -- Choose_Class --
   ------------------

   function Choose_Class
     (Actor : Chaos.Db.Actor.Actor_Type)
      return Natural
   is
      use Chaos.Db;
      Highest_Score : Integer := Integer'First;
      Best_Class    : Chaos.Db.Class_Reference :=
                        Chaos.Db.Null_Class_Reference;
   begin
      for Class of Chaos.Db.Class.Select_By_Tag loop
         declare
            Class_Score : Natural := 0;
         begin
            for Bonus of
              Chaos.Db.Ability_Score.Select_By_Able
                (Class.Reference)
            loop
               declare
                  use Chaos.Db.Ability_Score;
                  Score : constant Ability_Score_Type :=
                            Get_By_Ability_Score
                              (Actor.Reference, Bonus.Ability);
               begin
                  Class_Score := Class_Score
                    + Score.Value * (10 - Bonus.Value ** 2);
               end;
            end loop;
--              Ada.Text_IO.Put_Line
--                ("Actor" & Db.To_String (Actor_Reference'(Actor.Reference))
--                 & " /" & Class.Tag
--                 & " score"
--                 & Natural'Image (Class_Score));

            if Class_Score > Highest_Score then
               Highest_Score := Class_Score;
               Best_Class := Class.Reference;
            end if;
         end;
      end loop;
      if Highest_Score > 175 then
         Actor.Set_Class (Best_Class);
      end if;

      return Highest_Score;

   end Choose_Class;

   ----------------------
   -- Random_Character --
   ----------------------

   procedure Random_Character is
      use Chaos.Db;
      Actor : Chaos.Db.Actor.Actor_Type := Chaos.Db.Actor.Create;
      Score : Natural;
   begin
      for Ability of Chaos.Db.Ability.Select_By_Top_Record loop
         declare
            Value : constant Positive := Chaos.Dice.Roll (3, 6, 0);
         begin
            Chaos.Db.Ability_Score.Create
              (Able    => Actor.Reference,
               Ability => Ability.Reference,
               Value   => Value);
         end;
      end loop;

      Score := Choose_Class (Actor);

      Actor.Set_Level (1);
      Actor.Set_Size (Medium);
      Actor.Set_Origin (Normal);

--        for Class_Ability of
--          Chaos.Db.Ability_Score.Select_By_Able
--            (Chaos.Db.Class.Get (Actor.Class).Reference)
--        loop
--           declare
--              Actor_Ability : Ability_Score.Ability_Score_Type :=
--                                Chaos.Db.Ability_Score.Get_By_Ability_Score
--                                  (Actor.Reference, Class_Ability.Ability);
--           begin
--              Actor_Ability.Set_Value
--                (Actor_Ability.Value + Class_Ability.Value);
--           end;
--        end loop;

      declare
         use Ada.Text_IO;
         Current_Col : Count := 20;
      begin
         Put (Natural'Image (Score));

         Set_Col (6);

         if Actor.Class /= Null_Class_Reference then
            Put (Chaos.Db.Class.Get (Actor.Class).Tag);
         else
            Put ("normal");
         end if;

         for Ability of Chaos.Db.Ability.Select_By_Top_Record loop
            declare
               Score : constant Ability_Score.Ability_Score_Type :=
                         Ability_Score.Get_By_Ability_Score
                           (Actor.Reference, Ability.Reference);
            begin
               Set_Col (Current_Col);
               Current_Col := Current_Col + 7;
               Put (Ability.Tag & Natural'Image (Score.Value));
            end;
         end loop;
         New_Line;
      end;

   end Random_Character;

end Chaos.Characters;
