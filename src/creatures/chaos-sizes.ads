package Chaos.Sizes is

   type Chaos_Size is (Tiny, Small, Medium, Large, Gigantic);

   type Chaos_Size_Interface is limited interface;

   function Size (Item : Chaos_Size_Interface) return Chaos_Size
                  is abstract;

end Chaos.Sizes;
