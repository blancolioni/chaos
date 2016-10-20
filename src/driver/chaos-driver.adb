with Ada.Text_IO;

with Lith.Objects;

with Chaos.Actors;
with Chaos.Areas;
with Chaos.Classes;
with Chaos.Configuration;
with Chaos.Creatures.Quick;
with Chaos.Dice;
with Chaos.Expressions;
with Chaos.Game;
with Chaos.Items;
with Chaos.Localisation;
with Chaos.Parser;
with Chaos.Vision;

--  with Chaos.Classes;
with Chaos.Races;

--  with Chaos.Areas.Import;
--  with Chaos.Creatures.Import;
--  with Chaos.Dialog.Import;
--
--  with Chaos.Creatures.Quick;

with Chaos.Resources.Tlk;

--  with Chaos.Creatures.Reports;

with Chaos.UI.Text_UI;
with Chaos.Xi_UI;

with Chaos.Paths;
with Chaos.Infinity_Engine;

procedure Chaos.Driver is
   Test_Only : constant Boolean := True;
   Text_UI : constant Boolean := False;

   Tlk  : Chaos.Resources.Tlk.Tlk_Resource;
begin
   Chaos.Infinity_Engine.Read_Infinity_Config;
   Chaos.Configuration.Read_Configuration;

   declare
      Start : constant Lith.Objects.Object :=
                Chaos.Parser.Load_Script
                  (Chaos.Paths.Config_File ("start.script"));
   begin
      Ada.Text_IO.Put_Line
        (Chaos.Expressions.Store.Show (Start));
      Ada.Text_IO.Put_Line
        (Chaos.Expressions.Store.Show
           (Chaos.Expressions.Store.Evaluate (Start)));
   end;

   declare
      Expr : constant Lith.Objects.Object :=
               Chaos.Vision.To_Expression (Chaos.Vision.Low_Light);
      Roll : constant Lith.Objects.Object :=
               Chaos.Dice.To_Expression
                 (Chaos.Dice.Parse_Die_Roll ("1d6+1"));
   begin
      Ada.Text_IO.Put_Line
        (Chaos.Expressions.Store.Show (Expr));
      Ada.Text_IO.Put_Line
        (Chaos.Expressions.Store.Show (Roll));
      for I in 1 .. 10 loop
         declare
            use Chaos.Expressions;
         begin
            Ada.Text_IO.Put
              (Store.Show (Store.Evaluate (Roll)) & " ");
         end;
      end loop;
   end;

   Ada.Text_IO.New_Line;
   Tlk.Open ("DIALOG  ",
             Chaos.Infinity_Engine.Infinity_Path
             & "dialog.tlk");
   Tlk.Load;
   Tlk.Close;

   if Test_Only then
      declare
         UI : constant Chaos.UI.Chaos_UI :=
                Chaos.UI.Text_UI.Create;
      begin
         Chaos.UI.Set_Current_UI (UI);

         declare
            Protagonist : constant Chaos.Creatures.Chaos_Creature :=
                            Chaos.Creatures.Quick.Quick_Creature
                              ("Aramael",
                               Chaos.Races.Get ("eladrin"),
                               Chaos.Classes.Get ("wizard"));
         begin
            Chaos.Game.Create_Game (Protagonist);
         end;

         UI.Start;

         for I in 1 .. 3 loop
            Ada.Text_IO.Put_Line ("area script" & I'Img);
            Chaos.Game.Current_Game.Script_Round;
         end loop;

         for I in 1 .. 4 loop
            if I = 3 then
               declare
                  procedure Give_Book
                    (Creature : in out Creatures.Chaos_Creature_Record'Class);

                  ---------------
                  -- Give_Book --
                  ---------------

                  procedure Give_Book
                    (Creature : in out Creatures.Chaos_Creature_Record'Class)
                  is
                  begin
                     Creature.Replace_Item
                       (1, Chaos.Items.Create ("BOOK16"));
                  end Give_Book;

               begin
                  Chaos.Game.Current_Game.Party
                    .Party_Member (1).Creature.Update (Give_Book'Access);
               end;
            end if;

            Chaos.Game.Current_Game.Start_Dialog
              (Chaos.Game.Current_Game.Party.Party_Member (1),
               Chaos.Game.Current_Game.Area.Actor
                 (Chaos.Creatures.Get ("PHLYDI")));
         end loop;

         Chaos.Game.Current_Game.Start_Dialog
           (Chaos.Game.Current_Game.Party.Party_Member (1),
            Chaos.Game.Current_Game.Area.Actor
              (Chaos.Creatures.Get ("DREPPI")));

         Chaos.Game.Current_Game.Travel ("AR2616", "EXIT2600");

         Chaos.Game.Current_Game.Start_Dialog
           (Chaos.Game.Current_Game.Party.Party_Member (1),
            Chaos.Game.Current_Game.Area.Actor
              (Chaos.Creatures.Get ("FIREB1")));

         Chaos.Game.Current_Game.Travel ("AR2600", "EXIT2616");

         Chaos.Game.Current_Game.Start_Dialog
           (Chaos.Game.Current_Game.Party.Party_Member (1),
            Chaos.Game.Current_Game.Area.Actor
              (Chaos.Creatures.Get ("TETHTO")));

         Chaos.Game.Current_Game.Travel ("AR2616", "EXIT2600");

         Chaos.Game.Current_Game.Start_Dialog
           (Chaos.Game.Current_Game.Party.Party_Member (1),
            Chaos.Game.Current_Game.Area.Actor
              (Chaos.Creatures.Get ("FIREB1")));

         UI.Stop;
      end;
   else
      declare
         UI : constant Chaos.UI.Chaos_UI :=
             (if Text_UI
              then Chaos.UI.Text_UI.Create
              else Chaos.Xi_UI.Create);
      begin

         Chaos.UI.Set_Current_UI (UI);

         declare
            Protagonist : constant Chaos.Creatures.Chaos_Creature :=
                            Chaos.Creatures.Quick.Quick_Creature
                              ("Aramael",
                               Chaos.Races.Get ("eladrin"),
                               Chaos.Classes.Get ("wizard"));
         begin
            Chaos.Game.Create_Game (Protagonist);
         end;

         UI.Start;
      end;
   end if;

   Chaos.Expressions.Store.Report_State;

   Ada.Text_IO.Put_Line
     ("2000: " & Chaos.Localisation.Indexed_Text (2000));

end Chaos.Driver;
