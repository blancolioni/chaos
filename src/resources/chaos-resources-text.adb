package body Chaos.Resources.Text is

   ----------
   -- Line --
   ----------

   function Line
     (Resource : Text_Resource;
      Index    : Positive)
      return String
   is
   begin
      return Resource.Lines (Index);
   end Line;

   ----------------
   -- Line_Count --
   ----------------

   function Line_Count (Resource : Text_Resource) return Natural is
   begin
      return Resource.Lines.Last_Index;
   end Line_Count;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Resource : in out Text_Resource)
   is
      Index    : Natural := 0;
      Got_NL   : Boolean := False;
      X        : Word_8;
      Buffer   : String (1 .. 100) := (others => ' ');
      Length   : Natural := 0;
   begin
      while not Resource.End_Of_Resource loop
         Index := Index + 1;
         Resource.Get (X);
         if X = 10 or else X = 13 then
            if not Got_NL then
               Resource.Lines.Append (Buffer (1 .. Length));
               Length := 0;
            end if;
            Got_NL := True;
         else
            Got_NL := False;
            Length := Length + 1;
            Buffer (Length) := Character'Val (X);
         end if;
      end loop;

   end Load;

end Chaos.Resources.Text;
