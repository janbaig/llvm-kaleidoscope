#include <string>

// --- Initial Lexer --- 

enum Token {
  tok_eof = -1,

  // commands
  tok_def = -2,
  tok_extern = -3,

  // primary
  tok_identifier = -4,
  tok_number = -5
};

static std::string IdentifierStr; // used by tok_identifier
static double NumVal; // used by tok_numer

static int getToken() {
  static int LastChar = ' ';

  // skip whitespace
  while (isspace(LastChar)) {
    LastChar = getchar();
  }

  // handle alphanumeric (first char has to be alpha tho)
  if (isalpha(LastChar)) {
    IdentifierStr = LastChar;
    while(isalnum(LastChar = getchar()))
      IdentifierStr += LastChar;

    if (IdentifierStr == "def") 
      return tok_def;
    if (IdentifierStr == "extern")
      return tok_extern; 
    return tok_identifier;
  }

  // handle numbers
  if (isdigit(LastChar) || LastChar == '.') {
    std::string NumStr;
    do {
      NumStr += LastChar; 
      LastChar = getchar();
    } while (isdigit(LastChar) || LastChar == '.');
    
    NumVal = strtod(NumStr.c_str(), 0);
    return tok_number;
  }

  // handle comments
  if (LastChar == '#') {
    do
      LastChar = getchar();
    while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');
  
    if (LastChar != EOF)
      return getToken();
  } 

  // Do not consume the EOF 
  if (LastChar == EOF) 
    return tok_eof; 
  
  // return char as ASCII value
  int ThisChar = LastChar; 
  LastChar = getchar();
  return ThisChar;
}

// --- Future Driver ----

int main() {
    return 0;
}

