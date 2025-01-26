#include <string>
#include <vector>

// --- Lexer --- 

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

// --- Parser ---

class ExprAST {
public:
  virtual ~ExprAST() = default;  
};

class NumberExprAST : public ExprAST {
  double Val;
public:
  NumberExprAST(double Val) : Val(Val) {}
};

class VariableExprAST : public ExprAST {
  std::string Name;
public:
  VariableExprAST(const std::string &Name) : Name(Name) {}
};

class BinaryExprAST : public ExprAST {
  char Op;
  std::unique_ptr<ExprAST> LHS, RHS;
public:
  BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS, 
                std::unique_ptr<ExprAST> RHS) : 
    Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
};

class CallExprAST : public ExprAST {
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;
public:
  CallExprAST(const std::string &Callee, 
              std::vector<std::unique_ptr<ExprAST>> Args) :
    Callee(Callee), Args(std::move(Args)) {} 
};

class PrototypeAST {
  std::string Name;
  std::vector<std::string> Args;
public: 
  PrototypeAST(const std::string &Name, std::vector<std::string> Args) : 
    Name(Name), Args(Args) {}

  const std::string &getName() const { return  Name; }
};

class FunctionAST {
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<ExprAST> Body;
public:
  FunctionAST(std::unique_ptr<PrototypeAST> Proto, 
              std::unique_ptr<ExprAST> Body) : 
    Proto(std::move(Proto)), Body(std::move(Body)) {}
};

static int CurTok;
static int getNextToken() {
  return CurTok = getToken();
}
std::unique_ptr<ExprAST> LogError(const char *Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
  LogError(Str);
  return nullptr;
}

static std::unique_ptr<ExprAST> ParseNumberExpr() {
  auto Result = std::make_unique<NumberExprAST>(NumVal);
  getNextToken(); // consumes the number
  return std::move(Result);
}

static std::unique_ptr<ExprAST> ParseParenExpr() {
  getNextToken();
  auto V = ParseExpression(); 
  if (!V) 
    return nullptr;
  
  if (CurTok != ')') 
    return LogError("expected ')'");
  getNextToken();
  return V;
}

static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string IdName = IdentifierStr;
  getNextToken(); // consume the identifier

  // handle plain variable reference
  if (CurTok != '(') 
    return std::make_unique<VariableExprAST>(IdName);
  
  // handle call expr 
  getNextToken(); // consume the '('
  std::vector<std::unique_ptr<ExprAST>> Args;
  if (CurTok != ')') {
    while (true) {
      if (auto Arg = ParseExpression()) 
        Args.push_back(std::move(Arg));
      else 
        return nullptr;
      
      if (CurTok == ')')
        break;
      
      if (CurTok != ',')
        return LogError("expected ')' or '.' in argument list");
      getNextToken();
    }
  }
  
  getNextToken(); // consume the ')'
  return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

static std::unique_ptr<ExprAST> ParsePrimary() {
  switch (CurTok) {
  default:
    return LogError("unknown token when expecting an expression");
  case tok_identifier:
    return ParseIdentifierExpr();
  case tok_number:
    return ParseNumberExpr();
  case '(':
    return ParseParenExpr();
  }
}

// --- Future Driver ----

int main() {
    return 0;
}

