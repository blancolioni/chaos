package Chaos.Powers.Configure is

   procedure Read_Config;

   function Load_Power
     (Path : String)
      return Chaos_Power;

end Chaos.Powers.Configure;
