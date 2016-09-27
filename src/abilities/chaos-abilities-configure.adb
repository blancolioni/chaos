with Ada.Characters.Handling;

package body Chaos.Abilities.Configure is

   -------------------------------
   -- Configure_Ability_Changes --
   -------------------------------

   function Configure_Ability_Changes
     (Config : Tropos.Configuration;
      Child_Name : String := "abilities")
      return Ability_Score_Changes
   is
      Child : constant Tropos.Configuration :=
                (if Child_Name = ""
                 then Config
                 else Config.Child (Child_Name));
   begin
      return Changes : Ability_Score_Changes := (others => 0) do
         for A in Changes'Range loop
            declare
               Id : constant String :=
                      Ada.Characters.Handling.To_Lower
                        (Ability'Image (A));
            begin
               if Child.Contains (Id) then
                  Changes (A) :=
                    Ability_Score_Change
                      (Integer'(Child.Get (Id)));
               end if;
            end;
         end loop;
      end return;
   end Configure_Ability_Changes;

end Chaos.Abilities.Configure;
