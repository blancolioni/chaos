with WL.String_Maps;

with Chaos.Resources.Manager;

package body Chaos.Animations is

   type Animation_Factory_Access is
     access all Chaos_Animation_Factory'Class;

   Local_Animation_Factory : Animation_Factory_Access;

   package Animation_Vectors is
     new Ada.Containers.Vectors (Positive, Chaos_Animation);

   package Animation_Cache_Maps is
     new WL.String_Maps (Animation_Vectors.Vector, Animation_Vectors."=");

   Animation_Cache : Animation_Cache_Maps.Map;

   procedure Import_Animation
     (Code   : String);

   ---------------
   -- Add_Frame --
   ---------------

   procedure Add_Frame
     (Animation          : in out Chaos_Animation_Record;
      Width, Height      : Natural;
      Center_X, Center_Y : Natural;
      Frame              : Chaos.Resources.Bam.Frame_Pixel_Access)
   is
   begin
      Animation.Frames.Append
        ((Width, Height, Center_X, Center_Y, Frame));
   end Add_Frame;

   -----------
   -- Frame --
   -----------

   function Frame
     (Animation : Chaos_Animation_Record;
      Index     : Positive)
      return Chaos.Resources.Bam.Frame_Pixel_Access
   is
   begin
      return Animation.Frames.Element (Index).Pixels;
   end Frame;

   --------------------
   -- Frame_Center_X --
   --------------------

   function Frame_Center_X
     (Animation : Chaos_Animation_Record'Class;
      Index     : Positive)
      return Natural
   is
   begin
      return Animation.Frames.Element (Index).Centre_X;
   end Frame_Center_X;

   --------------------
   -- Frame_Center_Y --
   --------------------

   function Frame_Center_Y
     (Animation : Chaos_Animation_Record'Class;
      Index     : Positive)
      return Natural
   is
   begin
      return Animation.Frames.Element (Index).Centre_Y;
   end Frame_Center_Y;

   -----------------
   -- Frame_Count --
   -----------------

   function Frame_Count (Animation : Chaos_Animation_Record) return Natural is
   begin
      return Animation.Frames.Last_Index;
   end Frame_Count;

   ------------------
   -- Frame_Height --
   ------------------

   function Frame_Height
     (Animation : Chaos_Animation_Record'Class;
      Index     : Positive)
      return Natural
   is
   begin
      return Animation.Frames.Element (Index).Height;
   end Frame_Height;

   -----------------
   -- Frame_Width --
   -----------------

   function Frame_Width
     (Animation : Chaos_Animation_Record'Class;
      Index     : Positive)
      return Natural
   is
   begin
      return Animation.Frames.Element (Index).Width;
   end Frame_Width;

   -------------------
   -- Get_Animation --
   -------------------

   function Get_Animation
     (Code  : String;
      Index : Positive)
      return Chaos_Animation
   is
   begin
      if not Animation_Cache.Contains (Code) then
         Import_Animation (Code);
      end if;

      return Animation_Cache.Element (Code).Element (Index);
   end Get_Animation;

   ----------------------
   -- Import_Animation --
   ----------------------

   procedure Import_Animation
     (Code   : String)
   is
      Cycles : Animation_Vectors.Vector;
      Res    : access constant Chaos.Resources.Chaos_Resource'Class :=
                 Chaos.Resources.Manager.Load_Resource
                   (Chaos.Resources.To_Reference (Code),
                    Chaos.Resources.Bam_Resource);
   begin
      if Res = null then
         Res :=
           Chaos.Resources.Manager.Load_Resource
             (Chaos.Resources.To_Reference ("CHMT1G1"),
              Chaos.Resources.Bam_Resource);
      end if;

      declare
         Bam    : Chaos.Resources.Bam.Bam_Resource'Class renames
                    Chaos.Resources.Bam.Bam_Resource'Class
                      (Res.all);
      begin
         for Cycle_Index in 1 .. Bam.Cycle_Entries.Last_Index loop
            declare
               Cycle       : Cycle_Entry renames
                               Bam.Cycle_Entries.Element (Cycle_Index);
               Frame_Count : constant Natural := Cycle.Frame_Count;
               First_Frame : constant Positive := Cycle.First_Frame;
               Animation   : constant Chaos_Animation :=
                               Local_Animation_Factory.Create_Animation;
            begin
               for I in First_Frame .. First_Frame + Frame_Count - 1 loop
                  declare
                     Lookup_Index : constant Positive :=
                                      Bam.Frame_Lookup.Element (I);
                     Frame        : constant Chaos.Resources.Bam.Frame_Entry :=
                                      (if Lookup_Index < 2 ** 16
                                       then Bam.Frame_Entries (Lookup_Index)
                                       else Empty_Frame);
                     Height       : constant Natural := Natural (Frame.Height);
                     Width        : constant Natural := Natural (Frame.Width);
                     Center_X     : constant Natural :=
                                      Natural (Frame.Center_X);
                     Center_Y     : constant Natural :=
                                      Natural (Frame.Center_Y);
                  begin
                     Animation.Add_Frame
                       (Width, Height, Center_X, Center_Y,
                        Frame.Frame_Data);
                  end;
               end loop;

               Animation.Palette := Bam.Palette;

               Cycles.Append (Animation);

            end;
         end loop;

         Animation_Cache.Insert (Code, Cycles);
      end;
   end Import_Animation;

   -------------
   -- Palette --
   -------------

   function Palette
     (Animation : Chaos_Animation_Record'Class)
      return Chaos.Resources.Resource_Palette
   is
   begin
      return Animation.Palette;
   end Palette;

   ---------------------------
   -- Set_Animation_Factory --
   ---------------------------

   procedure Set_Animation_Factory
     (Factory : not null access Chaos_Animation_Factory'Class)
   is
   begin
      Local_Animation_Factory := Animation_Factory_Access (Factory);
   end Set_Animation_Factory;

end Chaos.Animations;
