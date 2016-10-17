with GCS.Lexer;
with GCS.Styles;

with Chaos.Parser.Tokens;              use Chaos.Parser.Tokens;

pragma Elaborate_All (GCS.Lexer);

private package Chaos.Parser.Lexical is
  new GCS.Lexer (Token              => Token,
                 Tok_None           => Tok_None,
                 Tok_End_Of_File    => Tok_End_Of_File,
                 Tok_Bad_Character  => Tok_Bad_Character,
                 Tok_Identifier     => Tok_Identifier,
                 Tok_String         => Tok_String_Constant,
                 Tok_Character      => Tok_String_Constant,
                 Tok_Integer        => Tok_None,
                 Tok_Float          => Tok_None,
                 First_Keyword      => Tok_If,
                 Keywords           => "if then else and or -",
                 First_Symbol       => Tok_Left_Brace,
                 Symbols            => "{ } [ ] ( ) , : ; . & / * # @ ' "
                                     & " + := => \ "
                                     & " = /= <= < >= > !",
                 Identifier_Start   => "abcdefghijklmnopqrstuvwxyz" &
                                       "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                                       "0123456789" &
                                       "_-$",
                 Identifier_Body    => "abcdefghijklmnopqrstuvwxyz" &
                                       "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                                       "0123456789" &
                                       "_-%$",
                 Line_Comment_Start => "--",
                 Block_Comment_Start => "/*",
                 Block_Comment_End   => "*/",
                 Properties         => (GCS.Styles.Single_Quote_Token => True,
                                        others => False));
