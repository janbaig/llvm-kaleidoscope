#include "kaleidoscopeJIT.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;
using namespace llvm::orc;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

enum Token {
  tok_eof = -1,

  // commands
  tok_def = -2,
  tok_extern = -3,

  // primary
  tok_identifier = -4,
  tok_number = -5,

  // control flow
  tok_if = -6, tok_then = -7, tok_else = -8,
  tok_for = -9, tok_in = -10,
};

static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal;             // Filled in if tok_number

static int gettok() {
  static int LastChar = ' ';

  // Skip any whitespace.
  while (isspace(LastChar))
    LastChar = getchar();

  if (isalpha(LastChar)) { // identifier: [a-zA-Z][a-zA-Z0-9]*
    IdentifierStr = LastChar;
    
    while (isalnum((LastChar = getchar())))
      IdentifierStr += LastChar;
    if (IdentifierStr == "def")
      return tok_def;
    if (IdentifierStr == "extern")
      return tok_extern;
    if (IdentifierStr == "if")
      return tok_if;
    if (IdentifierStr == "then")
      return tok_then; 
    if (IdentifierStr == "else")
      return tok_else;
    if (IdentifierStr == "for") 
      return tok_for; 
    if (IdentifierStr == "in")
      return tok_in;
    return tok_identifier;
  }

  if (isdigit(LastChar)) { // Number: [0-9.]+
    std::string NumStr;
    do {
      NumStr += LastChar;
      LastChar = getchar();
    } while (isdigit(LastChar) || LastChar == '.');

    NumVal = strtod(NumStr.c_str(), nullptr);
    return tok_number;
  }

  if (LastChar == '#') { // Comment until end of line.
    do
      LastChar = getchar();
    while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

    if (LastChar != EOF)
      return gettok();
  }

  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF)
    return tok_eof;

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  LastChar = getchar(); // returns an ascii int
  return ThisChar; // could be a binop
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

namespace {

class ExprAST {
public:
  virtual ~ExprAST() = default;

  virtual Value *codegen() = 0;
};

class NumberExprAST : public ExprAST {
  double Val;

public:
  NumberExprAST(double Val) : Val(Val) {}

  Value *codegen() override;
};

class VariableExprAST : public ExprAST {
  std::string Name;

public:
  VariableExprAST(const std::string &Name) : Name(Name) {}

  Value *codegen() override;
};

class BinaryExprAST : public ExprAST {
  char Op;
  std::unique_ptr<ExprAST> LHS, RHS;

public:
  BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
                std::unique_ptr<ExprAST> RHS)
      : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

  Value *codegen() override;
};

class CallExprAST : public ExprAST {
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;

public:
  CallExprAST(const std::string &Callee,
              std::vector<std::unique_ptr<ExprAST>> Args)
      : Callee(Callee), Args(std::move(Args)) {}

  Value *codegen() override;
};

// PrototypeAST - This class represents the "prototype" for a function,
// which captures its name, and its argument names (thus implicitly the number
// of arguments the function takes).
class PrototypeAST {
  std::string Name;
  std::vector<std::string> Args;

public:
  PrototypeAST(const std::string &Name, std::vector<std::string> Args)
      : Name(Name), Args(std::move(Args)) {}

  Function *codegen();
  const std::string &getName() const { return Name; }
};

class FunctionAST {
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<ExprAST> Body;

public:
  FunctionAST(std::unique_ptr<PrototypeAST> Proto,
              std::unique_ptr<ExprAST> Body)
      : Proto(std::move(Proto)), Body(std::move(Body)) {}

  Function *codegen();
};

class IfExprAST : public ExprAST {
  std::unique_ptr<ExprAST> Cond, Then, Else;

public: 
  IfExprAST(std::unique_ptr<ExprAST> Cond, 
            std::unique_ptr<ExprAST> Then, 
            std::unique_ptr<ExprAST> Else) 
          : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {} 
  
  Value *codegen() override;
};

class ForExprAST : public ExprAST {
  std::string VarName;
  std::unique_ptr<ExprAST> Start, End, Step, Body;

public:
  ForExprAST(const std::string &VarName, 
            std::unique_ptr<ExprAST> Start, 
            std::unique_ptr<ExprAST> End, 
            std::unique_ptr<ExprAST> Step, 
            std::unique_ptr<ExprAST> Body)
    : VarName(VarName), Start(std::move(Start)), End(std::move(End)), 
      Step(std::move(Step)), Body(std::move(Body)) {}

  Value *codegen() override;
};
} // end anonymous namespace

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

static int CurTok;
static int getNextToken() { return CurTok = gettok(); }

static std::map<char, int> BinopPrecedence;

static int GetTokPrecedence() {
  if (!isascii(CurTok))
    return -1;

  // Make sure it's a declared binop.
  int TokPrec = BinopPrecedence[CurTok];
  if (TokPrec <= 0)
    return -1;
  return TokPrec;
}

std::unique_ptr<ExprAST> LogError(const char *Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
  LogError(Str);
  return nullptr;
}

static std::unique_ptr<ExprAST> ParseExpression();

static std::unique_ptr<ExprAST> ParseNumberExpr() {
  auto Result = std::make_unique<NumberExprAST>(NumVal);
  getNextToken(); // consume the number
  return std::move(Result);
}

static std::unique_ptr<ExprAST> ParseParenExpr() {
  getNextToken(); // eat (.
  auto V = ParseExpression();
  if (!V)
    return nullptr;

  if (CurTok != ')')
    return LogError("expected ')'");
  getNextToken(); // eat ).
  return V;
}

static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string IdName = IdentifierStr;

  getNextToken(); // eat identifier.

  if (CurTok != '(') // Simple variable ref.
    return std::make_unique<VariableExprAST>(IdName);

  // Call expression
  getNextToken(); // eat (
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
        return LogError("Expected ')' or ',' in argument list");
      getNextToken(); // consume the ','
    }
  }

  // Eat the ')'.
  getNextToken();

  return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

static std::unique_ptr<ExprAST> ParseIfExpr() {
  getNextToken(); // eat the 'if' keyword

  // condition 
  auto Cond = ParseExpression();
  if (!Cond) 
    return nullptr; 
  
  if (CurTok != tok_then) 
    return LogError("expected 'then' keyword"); 
  getNextToken(); // eat the 'then' keyword
  
  auto Then = ParseExpression();
  if (!Then) 
    return nullptr; 
  
  if (CurTok != tok_else)
    return LogError("expected 'else' keyword"); // too strict, might tweak later down the line
  getNextToken(); // eat the 'then' keyword

  auto Else = ParseExpression();
  if (!Else) 
    return nullptr; 
  
  return std::make_unique<IfExprAST>(std::move(Cond), std::move(Then), std::move(Else));
}

static std::unique_ptr<ExprAST> ParseForExpr() {
  getNextToken(); // eat the 'for' keyword

  if (CurTok != tok_identifier) 
    return LogError("expected identifier after 'for'");
  
  std::string IdName = IdentifierStr;
  getNextToken(); // eat the identifier 

  if (CurTok != '=')
    return LogError("expected '=' after 'for'");
  getNextToken(); // eat '='

  auto Start = ParseExpression();
  if (!Start)
    return nullptr;
  if (CurTok != ',') 
    return LogError("expected ',' after for start value");
  getNextToken();

  auto End = ParseExpression(); // condition
  if (!End) 
    return nullptr; 
  
  // the step value is optional, defaults to 1.0 
  std::unique_ptr<ExprAST> Step; 
  if (CurTok == ',') {
    getNextToken(); // consumes the ','
    Step = ParseExpression(); 
    if (!Step) 
      return nullptr;
  }

  if (CurTok != tok_in) 
    return LogError("expected 'in' after for");
  getNextToken(); // eat the 'in'

  auto Body = ParseExpression();
  if (!Body)
    return nullptr; 
  
  return std::make_unique<ForExprAST>(IdName, std::move(Start), 
                                      std::move(End), std::move(Step), 
                                      std::move(Body));
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
  case tok_if: 
    return ParseIfExpr();
  case tok_for: 
    return ParseForExpr();
  }
}

static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS) {
  // If this is a binop, find its precedence.
  while (true) {
    int TokPrec = GetTokPrecedence();

    // If this is a binop that binds at least as tightly as the current binop,
    // consume it, otherwise we are done.
    if (TokPrec < ExprPrec)
      return LHS;

    // Okay, we know this is a binop.
    int BinOp = CurTok;
    getNextToken(); // eat binop

    // Parse the primary expression after the binary operator.
    auto RHS = ParsePrimary();
    if (!RHS)
      return nullptr;

    // If BinOp binds less tightly with RHS than the operator after RHS, let
    // the pending operator take RHS as its LHS.
    int NextPrec = GetTokPrecedence();
    if (TokPrec < NextPrec) {
      RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
      if (!RHS)
        return nullptr;
    }

    // Merge LHS/RHS.
    LHS =
        std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
  }
}

static std::unique_ptr<ExprAST> ParseExpression() {
  auto LHS = ParsePrimary();
  if (!LHS)
    return nullptr;

  return ParseBinOpRHS(0, std::move(LHS));
}

static std::unique_ptr<PrototypeAST> ParsePrototype() {
  if (CurTok != tok_identifier)
    return LogErrorP("Expected function name in prototype");

  std::string FnName = IdentifierStr;
  getNextToken();

  if (CurTok != '(')
    return LogErrorP("Expected '(' in prototype");

  std::vector<std::string> ArgNames;
  while (getNextToken() == tok_identifier)
    ArgNames.push_back(IdentifierStr);
  if (CurTok != ')')
    return LogErrorP("Expected ')' in prototype");

  // success.
  getNextToken(); // eat ')'.

  return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}

static std::unique_ptr<FunctionAST> ParseDefinition() {
  getNextToken(); // eat def.
  auto Proto = ParsePrototype();
  if (!Proto)
    return nullptr;

  if (auto E = ParseExpression())
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  return nullptr;
}

static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
  if (auto E = ParseExpression()) {
    // Make an anonymous proto.
    auto Proto = std::make_unique<PrototypeAST>("__anon_expr",
                                                 std::vector<std::string>());
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  }
  return nullptr;
}

static std::unique_ptr<PrototypeAST> ParseExtern() {
  getNextToken(); // eat extern.
  return ParsePrototype();
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<IRBuilder<>> Builder;

static std::map<std::string, Value *> NamedValues;
static std::unique_ptr<KaleidoscopeJIT> TheJIT;
static std::unique_ptr<FunctionPassManager> TheFPM;
static std::unique_ptr<LoopAnalysisManager> TheLAM;
static std::unique_ptr<FunctionAnalysisManager> TheFAM;
static std::unique_ptr<CGSCCAnalysisManager> TheCGAM;
static std::unique_ptr<ModuleAnalysisManager> TheMAM;
static std::unique_ptr<PassInstrumentationCallbacks> ThePIC;
static std::unique_ptr<StandardInstrumentations> TheSI;

static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
static ExitOnError ExitOnErr;

Value *LogErrorV(const char *Str) {
  LogError(Str);
  return nullptr;
}

Function *getFunction(std::string Name) {
  if (auto *F = TheModule->getFunction(Name))
    return F;

  // The function dne in the current module, check if we can codegen it's decl
  // from an existing prototype (defined in another module)
  auto FI = FunctionProtos.find(Name);
  if (FI != FunctionProtos.end())
    return FI->second->codegen();
  
  return nullptr;
}

Value *NumberExprAST::codegen() {
  return ConstantFP::get(*TheContext, APFloat(Val));
}

Value *VariableExprAST::codegen() {
  // Look this variable up in the function.
  Value *V = NamedValues[Name];
  if (!V)
    return LogErrorV("Unknown variable name");
  return V;
}

Value *BinaryExprAST::codegen() {
  Value *L = LHS->codegen();
  Value *R = RHS->codegen();
  if (!L || !R)
    return nullptr;

  switch (Op) {
  case '+':
    return Builder->CreateFAdd(L, R, "addtmp");
  case '-':
    return Builder->CreateFSub(L, R, "subtmp");
  case '*':
    return Builder->CreateFMul(L, R, "multmp");
  case '<':
    L = Builder->CreateFCmpULT(L, R, "cmptmp");
    // Convert bool 0/1 to double 0.0 or 1.0
    return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
  default:
    return LogErrorV("invalid binary operator");
  }
}

Value *CallExprAST::codegen() {
  // Look up the name in the global module table.
  Function *CalleeF = getFunction(Callee);
  if (!CalleeF)
    return LogErrorV("Unknown function referenced");

  // If argument mismatch error.
  if (CalleeF->arg_size() != Args.size())
    return LogErrorV("Incorrect # arguments passed");

  std::vector<Value *> ArgsV;
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    ArgsV.push_back(Args[i]->codegen());
    if (!ArgsV.back())
      return nullptr;
  }

  return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

Function *PrototypeAST::codegen() {
  // Make the function type:  double(double,double) etc.
  std::vector<Type *> Doubles(Args.size(), Type::getDoubleTy(*TheContext));
  FunctionType *FT =
      // get(...) because we want to reuse the type pointer if it already
      // exists in the context; otherwise, create a new one
      FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);

  Function *F =
      Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

  // Set names for all arguments.
  unsigned Idx = 0;
  for (auto &Arg : F->args())
    Arg.setName(Args[Idx++]);

  return F;
}

Function *FunctionAST::codegen() {
  auto &P = *Proto; 
  FunctionProtos[Proto->getName()] = std::move(Proto);
  Function *TheFunction = getFunction(P.getName());
  if (!TheFunction) 
    return nullptr;

  // Create a new basic block to start insertion into.
  BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
  Builder->SetInsertPoint(BB);

  // Record the function arguments in the NamedValues map.
  NamedValues.clear();
  for (auto &Arg : TheFunction->args())
    NamedValues[std::string(Arg.getName())] = &Arg;

  if (Value *RetVal = Body->codegen()) {
    // Finish off the function.
    Builder->CreateRet(RetVal);

    // Validate the generated code, checking for consistency.
    verifyFunction(*TheFunction);
    
    // Optimize the function 
    TheFPM->run(*TheFunction, *TheFAM);
    
    return TheFunction;
  }

  // Error reading body, remove function.
  TheFunction->eraseFromParent();
  return nullptr;
}

Value *IfExprAST::codegen() {
  // emits inside entryBB
  Value *CondV = Cond->codegen();
  if (!Cond) 
    return nullptr; 
  
  // Compare condition != 0, if yes then true, else false bool value
  CondV = Builder->CreateFCmpONE(CondV, ConstantFP::get(*TheContext, APFloat(0.0)), "ifcond");
  
  // the parent would be whatever function contained the ifExpr 
  Function *TheFunction = Builder->GetInsertBlock()->getParent();

  // has Function because we wanna insert this at the end of the current BB
  BasicBlock *ThenBB = BasicBlock::Create(*TheContext, "then", TheFunction);
  BasicBlock *ElseBB = BasicBlock::Create(*TheContext, "else");
  BasicBlock *MergeBB = BasicBlock::Create(*TheContext, "ifcont");

  // create the conditional branch, inside the entryBB
  Builder->CreateCondBr(CondV, ThenBB, ElseBB);

  // emit into the 'then' block 
  Builder->SetInsertPoint(ThenBB);
  Value *ThenV = Then->codegen(); 
  if (!ThenV) 
    return nullptr; 
  Builder->CreateBr(MergeBB);
  ThenBB = Builder->GetInsertBlock(); // whatever BB the Then->codegen stopped at 

  // emit into the 'else' block 
  TheFunction->insert(TheFunction->end(), ElseBB);
  Builder->SetInsertPoint(ElseBB);
  Value *ElseV = Else->codegen();
  if (!ElseV) 
    return nullptr;
  Builder->CreateBr(MergeBB);
  ElseBB = Builder->GetInsertBlock();

  // emit into the 'merge' block
  TheFunction->insert(TheFunction->end(), MergeBB);
  Builder->SetInsertPoint(MergeBB);
  PHINode *PN = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, "iftmp");
  PN->addIncoming(ThenV, ThenBB);
  PN->addIncoming(ElseV, ElseBB);
  return PN;
};

Value *ForExprAST::codegen() {
  Value *StartVal = Start->codegen(); // current BB
  if (!StartVal) 
    return nullptr; 
  
  // Function that contained the ForExpr
  Function *TheFunction = Builder->GetInsertBlock()->getParent(); 
  // the current BB
  BasicBlock *PreheaderBB = Builder->GetInsertBlock(); 
  BasicBlock *LoopBB = BasicBlock::Create(*TheContext, "loop", TheFunction);
  Builder->CreateBr(LoopBB);// emit inside preheaderBB 

  // Start inserting into LoopBB 
  Builder->SetInsertPoint(LoopBB);
  PHINode *Variable = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, VarName);
  Variable->addIncoming(StartVal, PreheaderBB);

  // If the variable shadows an existing one
  Value *OldVal = NamedValues[VarName]; // OldVal is nullptr if no shadowing is needed
  NamedValues[VarName] = Variable;

  if (!Body->codegen()) 
    return nullptr; 
  
  // Emit the step value 
  Value *StepVal = nullptr; 
  if (Step) {
    StepVal = Step->codegen(); 
    if (!StepVal)
      return nullptr; 
  } else {
    // if not specified, use the detault 1.0
    StepVal = ConstantFP::get(*TheContext, APFloat(1.0));
  }

  // increment the Vatiable
  Value *NextVar = Builder->CreateFAdd(Variable, StepVal, "nextvar");

  // Emit the loop condition, returns FP value (if 0.0 then false, else true)
  Value *EndCond = End->codegen();
  if (!EndCond) 
    return nullptr; 
  
  // compare if EndCond != 0 (false), returns bool
  EndCond = Builder->CreateFCmpONE(EndCond, ConstantFP::get(*TheContext, APFloat(0.0)), "loopcond");

  // technically = to LoopBB, but above codegen'ing might have added more blocks 
  BasicBlock *LoopEndBB = Builder->GetInsertBlock(); 
  // Create the after block 
  BasicBlock *AfterBB = BasicBlock::Create(*TheContext, "afterloop", TheFunction); 
  Builder->CreateCondBr(EndCond, LoopBB, AfterBB); 

  Builder->SetInsertPoint(AfterBB); // future code emitted here 

  // Add new entry to PHI node created earlier 
  Variable->addIncoming(NextVar, LoopEndBB);

  // Restore the unshadowed variable, our shawdowed variable should not be in-scope beyond the ForExpr 
  if (OldVal)
    NamedValues[VarName] = OldVal; 
  else 
    NamedValues.erase(VarName); 
  
  // ForExprs always return 0.0
  return Constant::getNullValue(Type::getDoubleTy(*TheContext)); 
}

//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

static void InitializeModuleAndManagers() {
  // Open a new context, module and builder
  TheContext = std::make_unique<LLVMContext>();
  TheModule = std::make_unique<Module>("KaleidoscopeJIT", *TheContext);
  TheModule->setDataLayout(TheJIT->getDataLayout());
  Builder = std::make_unique<IRBuilder<>>(*TheContext);

  // LLVM optimization + analysis pass manager
  TheFPM = std::make_unique<FunctionPassManager>();
  TheLAM = std::make_unique<LoopAnalysisManager>();
  TheFAM = std::make_unique<FunctionAnalysisManager>();
  TheCGAM = std::make_unique<CGSCCAnalysisManager>();
  TheMAM = std::make_unique<ModuleAnalysisManager>();
  ThePIC = std::make_unique<PassInstrumentationCallbacks>();
  TheSI = std::make_unique<StandardInstrumentations>(*TheContext,
                                                    /*DebugLogging*/ true);
  TheSI->registerCallbacks(*ThePIC, TheMAM.get());

  // Add transform passes.
  TheFPM->addPass(InstCombinePass());
  TheFPM->addPass(ReassociatePass());
  TheFPM->addPass(GVNPass()); // common subsexpression elimination
  TheFPM->addPass(SimplifyCFGPass());

  // Register analysis passes used in these transform passes.
  PassBuilder PB;
  PB.registerModuleAnalyses(*TheMAM);
  PB.registerFunctionAnalyses(*TheFAM);
  PB.crossRegisterProxies(*TheLAM, *TheFAM, *TheCGAM, *TheMAM);
}

static void HandleDefinition() {
  if (auto FnAST = ParseDefinition()) {
    if (auto *FnIR = FnAST->codegen()) {
      fprintf(stderr, "Read function definition:\n");
      FnIR->print(errs());
      fprintf(stderr, "\n");
      ExitOnErr(TheJIT->addModule(
        ThreadSafeModule(std::move(TheModule), std::move(TheContext))));
      InitializeModuleAndManagers();
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleExtern() {
  if (auto ProtoAST = ParseExtern()) {
    if (auto *FnIR = ProtoAST->codegen()) {
      fprintf(stderr, "Read extern:\n");
      FnIR->print(errs());
      fprintf(stderr, "\n");
      FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST); // we dont' want to codegen the prototype
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (auto FnAST = ParseTopLevelExpr()) {
    if (auto *FnIR = FnAST->codegen()) {
      // Create a ResourceTracker to track JIT'd memory allocated to our
      // anonymous expression -- that way we can free it after executing.
      auto RT = TheJIT->getMainJITDylib().createResourceTracker();

      // After passing the module and context to the JIT, they are no longer
      // valid—treat their pointers as moved and unusable.
      auto TSM = ThreadSafeModule(std::move(TheModule), std::move(TheContext));
      ExitOnErr(TheJIT->addModule(std::move(TSM), RT));
      
      // Once the module has been added to the JIT it can no longer be modified, 
      // so we also open a new module to hold subsequent code
      InitializeModuleAndManagers();

      // Search the JIT for the __anon_expr symbol.
      auto ExprSymbol = ExitOnErr(TheJIT->lookup("__anon_expr"));

      // Get the symbol's address and cast it to the right type (takes no
      // arguments, returns a double) so we can call it as a native function.
      double (*FP)() = ExprSymbol.getAddress().toPtr<double (*)()>();
      fprintf(stderr, "Evaluated to %f\n", FP());

      // Delete the anonymous expression module from the JIT.
      ExitOnErr(RT->remove());
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void MainLoop() {
  while (true) {
    fprintf(stderr, "ready> ");
    switch (CurTok) {
    case tok_eof:
      return;
    case ';': // ignore top-level semicolons.
      getNextToken();
      break;
    case tok_def:
      HandleDefinition();
      break;
    case tok_extern:
      HandleExtern();
      break;
    default:
      HandleTopLevelExpression();
      break;
    }
  }
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main() {
  InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();
  InitializeNativeTargetAsmParser();

  BinopPrecedence['<'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 20;
  BinopPrecedence['*'] = 40; // highest.

  // Prime the first token.
  fprintf(stderr, "ready> ");
  getNextToken();

  TheJIT = ExitOnErr(KaleidoscopeJIT::Create());

  // Make the module, which holds all the code.
  InitializeModuleAndManagers();

  MainLoop();

  // Print out all of the generated code, we discharge modules tho..? whats the point here?
  TheModule->print(errs(), nullptr);

  return 0;
}
