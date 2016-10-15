with Lith.Objects.Symbols;

package body Chaos.Expressions.Import.Objects is

   -------------------
   -- Import_Object --
   -------------------

   procedure Import_Object
     (Team      : Natural;
      Faction   : Natural;
      EA        : Natural;
      General   : Natural;
      Race      : Natural;
      Class     : Natural;
      Specific  : Natural;
      Gender    : Natural;
      Alignment : Natural;
      Id_1      : Natural;
      Id_2      : Natural;
      Id_3      : Natural;
      Id_4      : Natural;
      Id_5      : Natural;
      Name      : String)
   is
      pragma Unreferenced (Team, Faction, EA, General, Race, Class,
                           Specific, Gender, Alignment,
                           Id_1, Id_2, Id_3, Id_4, Id_5);
   begin
      if Name /= "" then
         Store.Push
           (Lith.Objects.Symbols.Get_Symbol ("chaos-object-with-code"));
         Store.Push (Lith.Objects.Symbols.Get_Symbol (Name));
         Store.Create_List (2);
         Store.Push (Store.Pop, Lith.Objects.Secondary);

         --           declare
--              use type Chaos.Objects.Chaos_Object;
--              Object : constant Chaos.Objects.Chaos_Object :=
--                         Chaos.Objects.Search.Find_Object
--                           (Name);
--           begin
--              if Object = null then
--                 Chaos.Logging.Log ("SCRIPT", "unknown object: " & Name);
--                 Store.Push (Lith.Objects.Nil, Lith.Objects.Secondary);
--              else
--                 Store.Push (Object.To_Expression, Lith.Objects.Secondary);
--              end if;
--           end;
      else
         Store.Push (Lith.Objects.Symbols.Quote_Symbol);
         Store.Push_Nil;
         Store.Create_List (2);
         Store.Push (Store.Pop, Lith.Objects.Secondary);
      end if;
   end Import_Object;

end Chaos.Expressions.Import.Objects;
