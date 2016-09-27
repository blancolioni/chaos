with Tropos;

package Chaos.Abilities.Configure is

   function Configure_Ability_Changes
     (Config : Tropos.Configuration;
      Child_Name : String := "abilities")
      return Ability_Score_Changes;

end Chaos.Abilities.Configure;
