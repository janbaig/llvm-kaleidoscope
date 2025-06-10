//===- KaleidoscopeJIT.h - A simple JIT for Kaleidoscope --------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Contains a simple JIT definition for use in the kaleidoscope tutorials.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_EXECUTIONENGINE_ORC_KALEIDOSCOPEJIT_H
#define LLVM_EXECUTIONENGINE_ORC_KALEIDOSCOPEJIT_H

#include "llvm/ADT/StringRef.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutorProcessControl.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/Orc/Shared/ExecutorSymbolDef.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include <memory>

namespace llvm {
namespace orc {

class KaleidoscopeJIT {
private:
  std::unique_ptr<ExecutionSession> ES;       // Manages JIT runtime state
  DataLayout DL;                              // Target data layout (e.g., x86_64)
  MangleAndInterner Mangle;                   // Mangles symbol names (e.g., "foo" → "_Z3foov")
  IRCompileLayer CompileLayer;                // Compiles IR → object files
  RTDyldObjectLinkingLayer ObjectLayer;       // Links object files into memory
  JITDylib &MainJD;                           // Dynamic library - "symbol table" for JIT-compiled code

public:

  KaleidoscopeJIT(std::unique_ptr<ExecutionSession> ES, 
                  JITTargetMachineBuilder JTMB, 
                  DataLayout DL) 
    : ES(std::move(ES)), DL(std::move(DL)), Mangle(*this->ES, this->DL),
    CompileLayer(*this->ES, ObjectLayer, 
                std::make_unique<ConcurrentIRCompiler>(std::move(JTMB))),
    ObjectLayer(*this->ES, 
      []() { return std::make_unique<SectionMemoryManager>(); }),
    MainJD(this->ES->createBareJITDylib("<main>")) {

    MainJD.addGenerator( // Adds a generator to search for symbols in the current process
      cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(
        DL.getGlobalPrefix())));
  } 

  ~KaleidoscopeJIT() {
    if (auto Err = ES->endSession()) 
      ES->reportError(std::move(Err)); // if any errors, report it
  }

  static Expected<std::unique_ptr<KaleidoscopeJIT>> Create() {
    // We'll be running the JIT in-process, hence why the self ExecutorProcessControl
    auto EPC = SelfExecutorProcessControl::Create();
    if (!EPC) 
      return EPC.takeError();
    auto ES = std::make_unique<ExecutionSession>(std::move(*EPC));

    JITTargetMachineBuilder JTMB (
      ES->getExecutorProcessControl().getTargetTriple());
    
    auto DL = JTMB.getDefaultDataLayoutForTarget();
    if (!DL)
      return DL.takeError(); 
    
    return std::make_unique<KaleidoscopeJIT>(std::move(ES), std::move(JTMB), 
                                              std::move(*DL));
  }
  
  const DataLayout &getDataLayout() const { return DL; }
  
  JITDylib &getMainJITDylib() { return MainJD; }
  
  Error addModule(ThreadSafeModule TSM, ResourceTrackerSP RT = nullptr) {
    if (!RT) 
      RT = MainJD.getDefaultResourceTracker();
    return CompileLayer.add(RT, std::move(TSM));
  }

  Expected<ExecutorSymbolDef> lookup(StringRef Name) {
    return ES->lookup({ &MainJD }, Mangle(Name.str()));
  }
};

} // end namespace orc
} // end namespace llvm
#endif // LLVM_EXECUTIONENGINE_ORC_KALEIDOSCOPEJIT_H