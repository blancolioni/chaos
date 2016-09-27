with Ada.Text_IO;

with Chaos.Configuration;
with Chaos.Dice;
with Chaos.Expressions;
with Chaos.Expressions.Environments;
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

with Chaos.Paths;

procedure Chaos.Driver is
   Expr : constant Chaos.Expressions.Chaos_Expression :=
            Chaos.Vision.To_Expression (Chaos.Vision.Low_Light);
   Roll : constant Chaos.Expressions.Chaos_Expression :=
            Chaos.Dice.To_Expression
              (Chaos.Dice.Parse_Die_Roll ("1d6+1"));
   Tlk  : Chaos.Resources.Tlk.Tlk_Resource;
begin
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
                (Chaos.Expressions.Environments.Standard_Environment,
                 Start)));
   end;

   Ada.Text_IO.Put_Line
     (Chaos.Expressions.To_String (Expr));
   Ada.Text_IO.Put_Line
     (Chaos.Expressions.To_String (Roll));
   for I in 1 .. 10 loop
      Ada.Text_IO.Put
        (Chaos.Expressions.To_String
           (Chaos.Expressions.Evaluate
                (Chaos.Expressions.Environments.Standard_Environment,
                 Roll, "roll", Chaos.Expressions.No_Array)));
      Ada.Text_IO.Put (" ");
   end loop;
   Ada.Text_IO.New_Line;
   Tlk.Open (Chaos.Configuration.Infinity_Path
             & "dialog.tlk");
   Tlk.Load;
   Tlk.Close;

--     declare
--        Area : constant Chaos.Areas.Chaos_Area :=
--                 Chaos.Areas.Import.Import_Area ("AR2600");
--     begin
--        Ada.Text_IO.Put_Line
--          (Area.Identifier & ":"
--           & Natural'Image (Area.Squares_Across)
--           & " x"
--           & Natural'Image (Area.Squares_Down));
--     end;
--
--     declare
--        Area : constant Chaos.Areas.Chaos_Area :=
--                 Chaos.Areas.Import.Import_Area ("AR2602");
--     begin
--        Ada.Text_IO.Put_Line
--          (Area.Identifier & ":"
--           & Natural'Image (Area.Squares_Across)
--           & " x"
--           & Natural'Image (Area.Squares_Down));
--     end;
--
--     declare
--        Shank : constant Chaos.Creatures.Chaos_Creature :=
--                  Chaos.Creatures.Import.Import_Creature ("SHANK");
--     begin
--        Chaos.Creatures.Reports.Report (Shank);
--     end;
--
--     declare
--        Test : constant Chaos.Creatures.Chaos_Creature :=
--                 Chaos.Creatures.Quick.Quick_Creature
--                   ("Aramael Musitello",
--                    Chaos.Races.Get ("elf"),
--                    Chaos.Classes.Get ("fighter"));
--     begin
--        Chaos.Creatures.Reports.Report (Test);
--     end;

--     declare
--        Creature : constant access constant
--          Chaos.Resources.Chaos_Resource'Class :=
--            Chaos.Resources.Manager.Load_Resource
--              (Reference => "SHANK   ",
--               Res_Type  => Chaos.Resources.Creature_Resource);
--     begin
--        pragma Unreferenced (Creature);
--     end;

--     declare
--        Dialog : constant Chaos.Dialog.Chaos_Dialog :=
--                   Chaos.Dialog.Import.Import_Dialog ("SHANK");
--     begin
--        Ada.Text_IO.Put_Line (Dialog.Identifier);
--     end;

   Ada.Text_IO.Put_Line
     ("2000: " & Chaos.Localisation.Indexed_Text (2000));

   declare
      UI : constant Chaos.UI.Chaos_UI :=
             Chaos.UI.Text_UI.Create;
   begin
      UI.Start;
   end;

end Chaos.Driver;
