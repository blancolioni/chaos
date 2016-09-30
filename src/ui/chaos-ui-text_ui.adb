with Ada.Text_IO;

with Chaos.Configuration;
with Chaos.Areas.Import;

with Chaos.Creatures.Quick;

with Chaos.Dialog;

with Chaos.Classes;
with Chaos.Races;

with Chaos.Party;
with Chaos.Game;

with Chaos.Images;

with Chaos.Actors.Visibility;

with Chaos.Expressions;
with Chaos.Resources.Tis;

package body Chaos.UI.Text_UI is

   type Text_Image_Container is
     new Chaos.Images.Root_Chaos_Image_Container with null record;

   overriding procedure Import_Tileset
     (Container : in out Text_Image_Container;
      Tis       : Chaos.Resources.Tis.Tis_Resource'Class)
   is null;

   Local_Image_Container : aliased Text_Image_Container;

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

   overriding procedure Display_Text
     (UI : in out Chaos_Text_UI;
      Text : String);

   overriding function Create_Image_Container
     (UI : Chaos_Text_UI)
      return Chaos.Images.Chaos_Image_Container
   is (Local_Image_Container'Access);

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
      Party       : constant Chaos.Party.Party_Type :=
                      Chaos.Party.Create_Party;
   begin
      Party.Add_Party_Member (Actor);

      Chaos.Game.Create_Game (Area, Party);
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

   ------------------
   -- Display_Text --
   ------------------

   overriding procedure Display_Text
     (UI   : in out Chaos_Text_UI;
      Text : String)
   is
      pragma Unreferenced (UI);
   begin
      Ada.Text_IO.Put (Text);
   end Display_Text;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (UI : in out Chaos_Text_UI)
   is
   begin
      UI.Display_Text ("Starting text interface ...");
      Chaos.Game.Current_Game.Start;
   end Start;

end Chaos.UI.Text_UI;
