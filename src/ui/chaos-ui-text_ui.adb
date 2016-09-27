with Ada.Text_IO;

with Chaos.Configuration;
with Chaos.Areas.Import;

with Chaos.Creatures.Quick;

with Chaos.Dialog;

with Chaos.Classes;
with Chaos.Races;

with Chaos.Actors.Visibility;

with Chaos.Expressions.Environments;

package body Chaos.UI.Text_UI is

   type Chaos_Text_UI is
     new Root_Chaos_UI with
      record
         Area : Chaos.Areas.Chaos_Area;
      end record;

   overriding procedure Start
     (UI : in out Chaos_Text_UI);

   overriding procedure Stop
     (UI : in out Chaos_Text_UI)
   is null;

   overriding procedure Show_Model
     (UI : in out Chaos_Text_UI;
      Model : not null access Root_UI_Model'Class)
   is null;

   ------------
   -- Create --
   ------------

   function Create return Chaos_UI is
      UI : Chaos_Text_UI;
      Protagonist : constant Chaos.Creatures.Chaos_Creature :=
                      Chaos.Creatures.Quick.Quick_Creature
                        ("Aramael",
                         Chaos.Races.Get ("elf"),
                         Chaos.Classes.Get ("wizard"));
      Area        : constant Chaos.Areas.Chaos_Area :=
                      Chaos.Areas.Import.Import_Area
                        (Chaos.Configuration.Start_Area);
      Actor       : constant Chaos.Actors.Chaos_Actor :=
                      Chaos.Actors.Create_Actor
                        (Protagonist, Area,
                         Area.To_Square
                           (Chaos.Configuration.Start_Location));
   begin
      UI.Area := Area;
      UI.Party.Add_Party_Member (Actor);

      declare
         use Chaos.Actors.Visibility;
         Group : Actor_Groups;

         procedure Report (Seen : Chaos.Actors.Chaos_Actor);

         ------------
         -- Report --
         ------------

         procedure Report (Seen : Chaos.Actors.Chaos_Actor) is
         begin
            Ada.Text_IO.Put_Line (Seen.Long_Name);
            if Seen.Creature.Has_Dialog then
               declare
                  Position : constant Chaos.Dialog.Dialog_Cursor :=
                               Seen.Creature.Dialog.Start;
               begin
                  Ada.Text_IO.Put_Line
                    (Chaos.Dialog.Text (Position));
               end;
            end if;
         end Report;

      begin
         Add_Actor_Can_See (Actor, Group);
         Iterate (Group, Report'Access);
      end;

      return new Chaos_Text_UI'(UI);
   end Create;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (UI : in out Chaos_Text_UI)
   is
   begin
      Chaos.Expressions.Execute
        (Chaos.Expressions.Environments.Standard_Environment,
         UI.Area.Script);
   end Start;

end Chaos.UI.Text_UI;
