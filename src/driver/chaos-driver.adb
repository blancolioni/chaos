with Ada.Text_IO;

with Chaos.Actors;
with Chaos.Classes;
with Chaos.Configuration;
with Chaos.Creatures.Quick;
with Chaos.Dice;
with Chaos.Expressions;
with Chaos.Expressions.Environments;
with Chaos.Expressions.Functions;
with Chaos.Game;
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
   Test_Only : constant Boolean := False;
   Text_UI : constant Boolean := False;

   Expr : constant Chaos.Expressions.Chaos_Expression :=
            Chaos.Vision.To_Expression (Chaos.Vision.Low_Light);
   Roll : constant Chaos.Expressions.Chaos_Expression :=
            Chaos.Dice.To_Expression
              (Chaos.Dice.Parse_Die_Roll ("1d6+1"));
   Tlk  : Chaos.Resources.Tlk.Tlk_Resource;
begin
   Chaos.Infinity_Engine.Read_Infinity_Config;
   Chaos.Configuration.Read_Configuration;

   declare
      Start : constant Chaos.Expressions.Chaos_Expression :=
                Chaos.Parser.Load_Script
                  (Chaos.Paths.Config_File ("start.script"));
   begin
      Ada.Text_IO.Put_Line
        (Chaos.Expressions.To_String (Start));
      Ada.Text_IO.Put_Line
        (Chaos.Expressions.To_String
           (Chaos.Expressions.Evaluate
                (Start, Chaos.Expressions.Environments.Standard_Environment)));
   end;

   Ada.Text_IO.Put_Line
     (Chaos.Expressions.To_String (Expr));
   Ada.Text_IO.Put_Line
     (Chaos.Expressions.To_String (Roll));
   for I in 1 .. 10 loop
      Ada.Text_IO.Put
        (Chaos.Expressions.To_String
           (Chaos.Expressions.Evaluate
                (Chaos.Expressions.Functions.Object_Method
                   (Roll, "roll"))));
      Ada.Text_IO.Put (" ");
   end loop;
   Ada.Text_IO.New_Line;
   Tlk.Open (Chaos.Infinity_Engine.Infinity_Path
             & "dialog.tlk");
   Tlk.Load;
   Tlk.Close;

   if Test_Only then
      declare
         UI : constant Chaos.UI.Chaos_UI :=
                Chaos.UI.Text_UI.Create;
      begin
         Chaos.UI.Set_Current_UI (UI);

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

   Ada.Text_IO.Put_Line
     ("2000: " & Chaos.Localisation.Indexed_Text (2000));

end Chaos.Driver;
