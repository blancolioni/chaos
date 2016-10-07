with Chaos.Resources.Area;
with Chaos.Resources.Wed;

package Chaos.Features.Import is

   function Import_Door
     (Area       : Chaos.Resources.Area.Area_Resource'Class;
      Wed        : Chaos.Resources.Wed.Wed_Resource'Class;
      Area_Index : Positive;
      Wed_Index  : Positive)
      return Chaos_Feature;

end Chaos.Features.Import;
