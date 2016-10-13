private package Chaos.Parser.Tokens is

   type Token is
      (Tok_None, Tok_End_Of_File, Tok_Bad_Character,
       Tok_Identifier, Tok_String_Constant,

       Tok_If, Tok_Then, Tok_Else, Tok_And, Tok_Or, Tok_Minus,

       Tok_Left_Brace, Tok_Right_Brace,
       Tok_Left_Bracket, Tok_Right_Bracket,
       Tok_Left_Paren, Tok_Right_Paren,
       Tok_Comma, Tok_Colon, Tok_Semicolon,
       Tok_Dot, Tok_Ampersand, Tok_Forward_Slash,
       Tok_Asterisk, Tok_Hash, Tok_At,
       Tok_Plus, Tok_Assign, Tok_Arrow, Tok_Lambda,
       Tok_Equal, Tok_Not_Equal,
       Tok_Less_Equal, Tok_Less, Tok_Greater_Equal, Tok_Greater
      );

end Chaos.Parser.Tokens;
