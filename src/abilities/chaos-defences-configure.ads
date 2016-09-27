with Tropos;

package Chaos.Defences.Configure is

   function Configure_Defence_Changes
     (Config      : Tropos.Configuration;
      Child_Name  : String := "defences")
      return Defence_Score_Changes;

end Chaos.Defences.Configure;
