with Ada.Characters.Handling;

package body Chaos.Defences.Configure is

   -------------------------------
   -- Configure_Defence_Changes --
   -------------------------------

   function Configure_Defence_Changes
     (Config      : Tropos.Configuration;
      Child_Name  : String := "defences")
      return Defence_Score_Changes
   is
      Child : constant Tropos.Configuration :=
                (if Child_Name = ""
                 then Config
                 else Config.Child (Child_Name));
   begin
      return Changes : Defence_Score_Changes := (others => 0) do
         for D in Changes'Range loop
            declare
               Id : constant String :=
                      Ada.Characters.Handling.To_Lower
                        (Defence'Image (D));
            begin
               if Child.Contains (Id) then
                  Changes (D) :=
                    Defence_Score_Change
                      (Integer'(Child.Get (Id)));
               end if;
            end;
         end loop;
      end return;
   end Configure_Defence_Changes;

end Chaos.Defences.Configure;
